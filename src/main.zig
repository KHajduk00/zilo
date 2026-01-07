//*** includes ***/
const std = @import("std");
const heap = @import("std").heap;
const mem = @import("std").mem;
const fs = @import("std").fs;

//*** defines ***//
fn CTRL_KEY(comptime k: u8) u8 {
    return k & 0x1f;
}

const ZILO_TAB_STOP = 8;

const editorKey = enum(u16) { BACKSPACE = 0x7f, ARROW_LEFT = 0x1002, ARROW_RIGHT = 0x1003, ARROW_UP = 0x1000, ARROW_DOWN = 0x1001, HOME_KEY = 0x1004, END_KEY = 0x1005, PAGE_UP = 0x1006, PAGE_DOWN = 0x1007, DEL_KEY = 0x1008 };

//*** data ***//
const zilo_version = "0.0.1";

var fnPressed: bool = false; // Global FN flag

const Erow = struct {
    size: usize, // Raw string size
    chars: []u8, // Raw string
    rsize: usize, // Rendered string size
    render: []u8, // Rendered string (with expanded tabs)
};

const EditorConfig = struct {
    cx: u16,
    cy: u16,
    rx: u16,
    rowoff: u16,
    coloff: u16,

    screenrows: u16,
    screencols: u16,

    numrows: u16,
    rows: []Erow,
    filename: ?[]const u8,
    dirty: u16,

    statusmsg: [80]u8,
    statusmsg_time: i64,

    orig_termios: std.posix.termios,
};

var E = EditorConfig{
    .cx = undefined,
    .cy = undefined,
    .rx = undefined,
    .rowoff = undefined,
    .coloff = undefined,

    .screenrows = undefined,
    .screencols = undefined,

    .numrows = 0,
    .rows = undefined,
    .filename = null,
    .dirty = 0,

    .statusmsg = undefined,
    .statusmsg_time = 0,

    .orig_termios = undefined,
};

const KeyAction = enum {
    Quit,
    NoOp,
};

//*** terminal ***//
// Function to restore the original terminal settings
export fn disableRawMode() void {
    std.posix.tcsetattr(std.fs.File.stdin().handle, .FLUSH, E.orig_termios) catch {
        std.debug.print("Error: Failed to restore terminal settings\n", .{});
        std.process.exit(1);
    };
}

fn die(msg: []const u8) noreturn {
    // Create buffer for stdout
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    stdout.writeAll("\x1b[2J") catch {};
    stdout.writeAll("\x1b[H") catch {};
    stdout.flush() catch {};

    std.debug.print("Error: {s}\n", .{msg});
    std.process.exit(1);
}

// Function to enable raw mode in the terminal
fn enableRawMode() !void {
    const stdin = std.fs.File.stdin().handle;

    E.orig_termios = std.posix.tcgetattr(stdin) catch {
        std.debug.print("Error: Could not get terminal attributes\n", .{});
        return error.TerminalError;
    };

    var raw = E.orig_termios;

    // Terminal mode flags:
    raw.lflag.ECHO = false; // Don't echo input characters
    raw.lflag.ICANON = false; // Read input byte-by-byte instead of line-by-line
    raw.lflag.ISIG = false; // Disable Ctrl-C and Ctrl-Z signals
    raw.iflag.IXON = false; // Disable Ctrl-S and Ctrl-Q signals
    raw.lflag.IEXTEN = false; // Disable Ctrl-V
    raw.iflag.ICRNL = false; // Fix Ctrl-M
    raw.oflag.OPOST = false; // Disable output processing
    raw.iflag.BRKINT = false; // Disable break processing
    raw.iflag.INPCK = false; // Disable parity checking
    raw.iflag.ISTRIP = false; // Disable stripping of 8th bit
    raw.cflag.CSIZE = .CS8; // Use 8-bit characters

    // Set read timeouts
    const VMIN = 5; // Minimum number of bytes before read returns
    const VTIME = 6; // Time to wait for input (tenths of seconds)
    raw.cc[VMIN] = 0; // Return immediately when any bytes are available
    raw.cc[VTIME] = 1; // Wait up to 0.1 seconds for input

    std.posix.tcsetattr(stdin, .FLUSH, raw) catch {
        std.debug.print("Error: Could not set terminal attributes\n", .{});
        return error.TerminalError;
    };
}

fn editorReadKey() !u16 {
    var buf: [1]u8 = undefined;
    const stdin_file = std.fs.File.stdin();
    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = stdin_file.reader(&stdin_buffer);
    const stdin = &stdin_reader.interface;

    while (true) {
        const n = try stdin.*.readSliceShort(buf[0..]);
        if (n == 1) break;
    }

    // Read escape sequence
    if (buf[0] == '\x1b') {
        var seq: [3]u8 = undefined;

        // Read first character of sequence
        const seq1 = try stdin.*.readSliceShort(seq[0..1]);
        if (seq1 != 1) return '\x1b';

        if (seq[0] == '[') {
            // Read second character
            const seq2 = try stdin.*.readSliceShort(seq[1..2]);
            if (seq2 != 1) return '\x1b';

            if (seq[1] >= '0' and seq[1] <= '9') {
                // Read third character for extended sequences
                const seq3 = try stdin.*.readSliceShort(seq[2..3]);
                if (seq3 != 1) return '\x1b';

                if (seq[2] == '~') {
                    // Handle Page Up/Down and Home/End keys
                    return switch (seq[1]) {
                        '1' => @intFromEnum(editorKey.HOME_KEY),
                        '3' => @intFromEnum(editorKey.DEL_KEY),
                        '4' => @intFromEnum(editorKey.END_KEY),
                        '5' => @intFromEnum(editorKey.PAGE_UP),
                        '6' => @intFromEnum(editorKey.PAGE_DOWN),
                        '7' => @intFromEnum(editorKey.HOME_KEY),
                        '8' => @intFromEnum(editorKey.END_KEY),
                        else => '\x1b',
                    };
                }
            } else {
                // Handle arrow keys
                return switch (seq[1]) {
                    'A' => @intFromEnum(editorKey.ARROW_UP),
                    'B' => @intFromEnum(editorKey.ARROW_DOWN),
                    'C' => @intFromEnum(editorKey.ARROW_RIGHT),
                    'D' => @intFromEnum(editorKey.ARROW_LEFT),
                    'H' => @intFromEnum(editorKey.HOME_KEY),
                    'F' => @intFromEnum(editorKey.END_KEY),
                    else => '\x1b',
                };
            }
        } else if (seq[0] == 'O') {
            const seq2 = try stdin.*.readSliceShort(seq[1..2]);
            if (seq2 != 1) return '\x1b';

            return switch (seq[1]) {
                'H' => @intFromEnum(editorKey.HOME_KEY),
                'F' => @intFromEnum(editorKey.END_KEY),
                else => '\x1b',
            };
        }
        return '\x1b';
    }
    return buf[0];
}

fn getWindowSize(rows: *u16, cols: *u16) !void {
    var ws: std.posix.winsize = undefined;
    const fd = std.posix.STDOUT_FILENO;

    if (std.posix.system.ioctl(fd, std.posix.T.IOCGWINSZ, @intFromPtr(&ws)) == -1 or ws.col == 0) {
        return error.TerminalSizeError;
    }

    rows.* = ws.row;
    cols.* = ws.col;
}

fn editorRowCxToRx(row: *const Erow, cx: u16) u16 {
    var rx: u16 = 0;
    var j: u16 = 0;
    while (j < cx) : (j += 1) {
        if (row.chars[j] == '\t') {
            rx += (ZILO_TAB_STOP - 1) - (rx % ZILO_TAB_STOP) + 1;
        } else {
            rx += 1;
        }
    }
    return rx;
}

fn editorAppendRow(allocator: mem.Allocator, s: []const u8) !void {
    const at = E.numrows;
    E.rows = try allocator.realloc(E.rows, E.numrows + 1);

    E.rows[at] = .{
        .size = s.len,
        .chars = try allocator.alloc(u8, s.len + 1),
        .rsize = 0,
        .render = &[_]u8{}, // We initialize as an empty slice (like Null in C)
    };

    @memcpy(E.rows[at].chars[0..s.len], s);
    E.rows[at].chars[s.len] = 0;

    try editorUpdateRow(allocator, &E.rows[at]);

    E.numrows += 1;
}

fn editorUpdateRow(allocator: mem.Allocator, row: *Erow) !void {
    var tabs: usize = 0;
    for (row.chars[0..row.size]) |c| {
        if (c == '\t') tabs += 1;
    }

    const extra_space_per_tab = ZILO_TAB_STOP - 1;
    const new_size = row.size + (tabs * extra_space_per_tab) + 1;

    if (row.render.len > 0) {
        allocator.free(row.render);
    }

    row.render = try allocator.alloc(u8, new_size);

    var idx: usize = 0;
    for (row.chars[0..row.size]) |c| {
        if (c == '\t') {
            row.render[idx] = ' ';
            idx += 1;

            while (idx % ZILO_TAB_STOP != 0) : (idx += 1) {
                row.render[idx] = ' ';
                idx += 1;
            }
        } else {
            row.render[idx] = c;
            idx += 1;
        }
    }

    row.render[row.size] = 0;
    row.rsize = row.size;
}

fn editorRowInsertChar(allocator: mem.Allocator, row: *Erow, at: usize, c: u8) !void {
    var insert_at = at;
    if (insert_at > row.size) {
        insert_at = row.size;
    }

    // Reallocate to make room for one more character
    row.chars = try allocator.realloc(row.chars, row.size + 1);

    // Move characters after insertion point one position to the right
    if (insert_at < row.size) {
        std.mem.copyBackwards(u8, row.chars[insert_at + 1 .. row.size + 1], row.chars[insert_at..row.size]);
    }

    row.size += 1;
    row.chars[insert_at] = c;

    try editorUpdateRow(allocator, row);
}

//*** editor operations ***//
fn editorInsertChar(allocator: mem.Allocator, c: u8) !void {
    if (E.cy == E.numrows) {
        try editorAppendRow(allocator, "");
    }
    try editorRowInsertChar(
        allocator,
        &E.rows[E.cy],
        @intCast(E.cx),
        c,
    );
    E.cx += 1;
}

//*** file i/o ***//
fn editorRowsToString(allocator: mem.Allocator) ![]u8 {
    var total_size: usize = 0;
    var i: usize = 0;
    while (i < E.numrows) : (i += 1) {
        total_size += E.rows[i].size + 1; // +1 for newline
    }

    const buf = try allocator.alloc(u8, total_size);
    var p: usize = 0;

    i = 0;
    while (i < E.numrows) : (i += 1) {
        @memcpy(buf[p .. p + E.rows[i].size], E.rows[i].chars[0..E.rows[i].size]);
        p += E.rows[i].size;
        buf[p] = '\n';
        p += 1;
    }

    return buf;
}

fn editorOpen(allocator: mem.Allocator, filename: []const u8) !void {
    if (E.filename) |old_filename| {
        allocator.free(old_filename);
    }

    E.filename = try allocator.dupe(u8, filename);

    const file = try fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();

    const file_size = try file.getEndPos();
    const file_contents = try allocator.alloc(u8, file_size);
    defer allocator.free(file_contents);

    var file_buffer: [4096]u8 = undefined;
    var file_reader = file.reader(&file_buffer);
    const reader = &file_reader.interface;

    try reader.*.readSliceAll(file_contents);

    var line_start: usize = 0;
    var line_end: usize = 0;

    while (line_end < file_size) {
        while (line_end < file_size and
            file_contents[line_end] != '\n' and
            file_contents[line_end] != '\r')
        {
            line_end += 1;
        }

        try editorAppendRow(allocator, file_contents[line_start..line_end]);

        if (line_end < file_size and file_contents[line_end] == '\r') line_end += 1;
        if (line_end < file_size and file_contents[line_end] == '\n') line_end += 1;
        line_start = line_end;
    }
}

fn editorSave(allocator: mem.Allocator) !void {
    if (E.filename == null) return;

    // Convert rows to a single buffer
    const buf = try editorRowsToString(allocator);
    defer allocator.free(buf);

    // Unwrap the optional filename
    const filename = E.filename.?; // Safe because we checked for null in first line

    // Open file for read/write, create if it doesn't exist
    if (fs.cwd().createFile(filename, .{ .read = true, .truncate = true })) |file| {
        defer file.close();
        // Try to write
        if (file.writeAll(buf)) |_| {
            editorSetStatusMessage("{d} bytes written to disk", .{buf.len});
            return;
        } else |_| {
            // Write failed, fall through to error message
        }
    } else |_| {
        // File creation failed, fall through to error message
    }

    // Single error message - reached if ANY operation failed
    editorSetStatusMessage("Can't save! I/O error", .{});
}

//*** output ***//
fn editorScroll() !void {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.rows[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

fn editorDrawStatusBar(list_writer: anytype) !void {
    try list_writer.writeAll("\x1b[7m"); // Invert colors
    var status: [80]u8 = undefined;
    var rstatus: [80]u8 = undefined;

    const status_slice = try std.fmt.bufPrint(&status, "{s} - {d} lines {s}", .{
        if (E.filename) |fname| fname else "[No Name]",
        E.numrows,
        if (E.dirty > 0) "modified" else "",
    });
    const rstatus_slice = try std.fmt.bufPrint(&rstatus, "{d}/{d}", .{ E.cy + 1, E.numrows });

    var len = status_slice.len;
    if (len > E.screencols) {
        len = E.screencols;
    }

    try list_writer.writeAll(status[0..len]);
    while (len < E.screencols) {
        if (E.screencols - len == rstatus_slice.len) {
            try list_writer.writeAll(rstatus_slice);
            break;
        } else {
            try list_writer.writeAll(" ");
            len += 1;
        }
    }
    try list_writer.writeAll("\x1b[m"); // Reset formatting
    try list_writer.writeAll("\r\n");
}

fn editorDrawMessageBar(list_writer: anytype) !void {
    try list_writer.writeAll("\x1b[K");

    var msg_len: usize = 0;
    while (msg_len < E.statusmsg.len and E.statusmsg[msg_len] != 0) {
        msg_len += 1;
    }
    if (msg_len > E.screencols) {
        msg_len = E.screencols;
    }
    if (msg_len > 0 and std.time.timestamp() - E.statusmsg_time < 5) {
        try list_writer.writeAll(E.statusmsg[0..msg_len]);
    }
}

fn editorRefreshScreen(allocator: mem.Allocator) !void {
    try editorScroll();

    var buf = std.array_list.Managed(u8).init(allocator);
    defer buf.deinit();
    var list_writer = buf.writer();

    try list_writer.writeAll("\x1b[?25l");
    try list_writer.writeAll("\x1b[H");

    try editorDrawRows(list_writer);
    try editorDrawStatusBar(list_writer);
    try editorDrawMessageBar(list_writer);

    try list_writer.print("\x1b[{d};{d}H", .{ (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1 });

    try list_writer.writeAll("\x1b[?25h");

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.writeAll(buf.items);
    try stdout.flush();
}

fn editorDrawRows(writer: anytype) !void {
    var y: usize = 0;
    while (y < E.screenrows) : (y += 1) {
        const filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 and y == E.screenrows / 3) {
                var welcome: [80]u8 = undefined;
                const welcome_msg = try std.fmt.bufPrint(&welcome, "Zilo editor -- version {s}", .{zilo_version});

                const display_len = @min(welcome_msg.len, E.screencols);
                const padding = (E.screencols - display_len) / 2;

                var i: usize = 0;
                while (i < padding) : (i += 1) {
                    try writer.writeAll(" ");
                }

                try writer.writeAll(welcome_msg[0..display_len]);
            } else {
                try writer.writeAll("~");
            }
        } else {
            const row = E.rows[filerow];
            var len = row.rsize;

            if (E.coloff >= len) {
                try writer.writeAll("");
            } else {
                const start = E.coloff;
                len -= start;
                if (len > E.screencols) len = E.screencols;
                try writer.writeAll(row.render[start .. start + len]);
            }
        }

        try writer.writeAll("\x1b[K");
        try writer.writeAll("\r\n");
    }
}

fn editorSetStatusMessage(comptime fmt: []const u8, args: anytype) void {
    const message = std.fmt.bufPrint(&E.statusmsg, fmt, args) catch {
        E.statusmsg[0] = 0;
        return;
    };
    if (message.len < E.statusmsg.len) {
        E.statusmsg[message.len] = 0;
    }
    E.statusmsg_time = std.time.timestamp();
}

//*** input ***//
fn editorMoveCursor(key: u16) void {
    var row: ?*Erow = if (E.cy < E.numrows) &E.rows[E.cy] else null;

    switch (key) {
        @intFromEnum(editorKey.ARROW_LEFT) => if (E.cx != 0) {
            E.cx -= 1;
        } else if (E.cy > 0) {
            E.cy -= 1;
            const prev_row = &E.rows[E.cy];
            E.cx = @as(u16, @min(prev_row.size, std.math.maxInt(u16)));
        },
        @intFromEnum(editorKey.ARROW_RIGHT) => {
            if (row) |r| {
                if (E.cx < r.size) {
                    E.cx += 1;
                } else if (E.cy < E.numrows - 1) {
                    E.cy += 1;
                    E.cx = 0;
                }
            }
        },
        @intFromEnum(editorKey.ARROW_UP) => if (E.cy != 0) {
            E.cy -= 1;
        },
        @intFromEnum(editorKey.ARROW_DOWN) => if (E.numrows > 0 and E.cy < E.numrows - 1) {
            E.cy += 1;
        },
        else => {},
    }

    row = if (E.cy < E.numrows) &E.rows[E.cy] else null;
    const rowlen = if (row) |r| r.size else 0;
    if (E.cx > rowlen) {
        E.cx = @as(u16, @min(rowlen, std.math.maxInt(u16)));
    }
}

fn editorProcessKeypress(allocator: mem.Allocator) !KeyAction {
    const c = try editorReadKey();

    return switch (c) {
        '\r' => .NoOp, //TODO

        CTRL_KEY('q') => {
            var stdout_buffer: [1024]u8 = undefined;
            var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
            const stdout = &stdout_writer.interface;

            try stdout.writeAll("\x1b[2J");
            try stdout.writeAll("\x1b[H");
            try stdout.flush();
            return .Quit;
        },
        CTRL_KEY('s') => {
            try editorSave(allocator);
            return .NoOp;
        },
        @intFromEnum(editorKey.HOME_KEY) => {
            E.cx = 0;
            return .NoOp;
        },
        @intFromEnum(editorKey.END_KEY) => {
            if (E.cy < E.numrows) {
                E.cx = @intCast(E.rows[E.cy].size);
            }
            return .NoOp;
        },

        @intFromEnum(editorKey.BACKSPACE), CTRL_KEY('h'), @intFromEnum(editorKey.DEL_KEY) => .NoOp, //TODO

        @intFromEnum(editorKey.PAGE_UP), @intFromEnum(editorKey.PAGE_DOWN) => {
            if (c == @intFromEnum(editorKey.PAGE_UP)) {
                E.cy = E.rowoff;
            } else {
                E.cy = E.rowoff + E.screenrows - 1;
                if (E.cy > E.numrows) E.cy = E.numrows;
            }

            var times = E.screenrows;
            while (times != 0) : (times -= 1) {
                editorMoveCursor(if (c == @intFromEnum(editorKey.PAGE_UP))
                    @intFromEnum(editorKey.ARROW_UP)
                else
                    @intFromEnum(editorKey.ARROW_DOWN));
            }
            return .NoOp;
        },

        @intFromEnum(editorKey.ARROW_UP), @intFromEnum(editorKey.ARROW_DOWN), @intFromEnum(editorKey.ARROW_LEFT), @intFromEnum(editorKey.ARROW_RIGHT) => {
            editorMoveCursor(c);
            return .NoOp;
        },

        CTRL_KEY('l'), '\x1b' => .NoOp,

        else => {
            if (fnPressed) {
                switch (c) {
                    'w' => editorMoveCursor(@intFromEnum(editorKey.ARROW_UP)),
                    'a' => editorMoveCursor(@intFromEnum(editorKey.ARROW_LEFT)),
                    's' => editorMoveCursor(@intFromEnum(editorKey.ARROW_DOWN)),
                    'd' => editorMoveCursor(@intFromEnum(editorKey.ARROW_RIGHT)),
                    else => try editorInsertChar(allocator, @intCast(c)),
                }
            } else {
                try editorInsertChar(allocator, @intCast(c));
            }
            return .NoOp;
        },
    };
}

//*** init ***//
fn initEditor() void {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.rows = &[0]Erow{};
    E.filename = null;
    E.dirty = 0;
    E.statusmsg[0] = 0;
    E.statusmsg_time = 0;

    getWindowSize(&E.screenrows, &E.screencols) catch {
        // Fallback values if we can't get terminal size for some reason
        E.screenrows = 24;
        E.screencols = 80;
    };
    E.screenrows -= 2;
}

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try enableRawMode();
    defer disableRawMode();
    initEditor();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 1) {
        try editorOpen(allocator, args[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-Q = quit | Ctrl-S = save", .{});

    while (true) {
        try editorRefreshScreen(allocator);
        switch (try editorProcessKeypress(allocator)) {
            .Quit => break,
            else => {},
        }
    }
}
