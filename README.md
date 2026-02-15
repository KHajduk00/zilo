# kz - A Lightweight Terminal Text Editor

A minimal yet powerful terminal-based text editor written in Zig, inspired by the kilo and micro editors. kz provides syntax highlighting, search functionality, and an intuitive interface for editing text files directly in your terminal.

## Features

- **Syntax Highlighting**: Built-in support for C/C++ with extensible syntax highlighting system
- **Incremental Search**: Fast file search with arrow key navigation through results
- **Tab Support**: Configurable tab stops with visual tab expansion
- **Status Bar**: Shows current file, line count, cursor position, and file type
- **Dirty Flag Tracking**: Warns you about unsaved changes before quitting
- **Line Numbers**: Implicit through the editor interface
- **Multiline Comment Support**: Proper highlighting for block comments
- **Keyboard Shortcuts**: Familiar Ctrl-based commands for common operations

## Requirements

- Zig 0.16.0

## Installation

### Building from Source

1. **Clone or download the repository**:
   ```bash
   git clone <repository-url>
   cd kz
   ```

2. **Build the project**:
   ```bash
   zig build
   ```

3. **The binary will be created in**:
   ```bash
   ./zig-out/bin/kz
   ```

### System-Wide Installation

To make `kz` available from anywhere in your system:

#### Option 1: Copy to a System Binary Directory

```bash
# Build the release binary
zig build

# Copy to /usr/local/bin (requires sudo)
sudo cp zig-out/bin/kz /usr/local/bin/

# Make it executable
sudo chmod +x /usr/local/bin/kz
```

#### Option 2: Add to User's Local Binary Directory

```bash
# Build the release binary
zig build

# Create local bin directory if it doesn't exist
mkdir -p ~/.local/bin

# Copy the binary
cp zig-out/bin/kz ~/.local/bin/

# Make it executable
chmod +x ~/.local/bin/kz

# Add to PATH if not already there (add to ~/.bashrc, ~/.zshrc, or ~/.profile)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Option 3: Create a Symbolic Link

```bash
# Build the release binary
zig build

# Create a symlink in /usr/local/bin
sudo ln -s "$(pwd)/zig-out/bin/kz" /usr/local/bin/kz
```

### Verify Installation

After installation, verify that kz is accessible:

```bash
which kz
kz --help  # (Note: currently shows usage when run without arguments)
```

## Usage

### Basic Usage

```bash
# Open an existing file
kz filename.txt

# Create and edit a new file
kz newfile.c

# Start with an empty buffer
kz
```

### Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl-Q` | Quit editor (prompts if unsaved changes) |
| `Ctrl-S` | Save file |
| `Ctrl-F` | Find/search in file |
| `Ctrl-H` | Delete character (same as Backspace) |
| `Arrow Keys` | Move cursor |
| `Page Up/Down` | Scroll by page |
| `Home` | Move to beginning of line |
| `End` | Move to end of line |
| `Enter` | Insert new line |
| `Backspace` | Delete character before cursor |
| `Delete` | Delete character at cursor |

### Search Mode

When in search mode (`Ctrl-F`):
- Type your search query
- Use `Arrow Keys` to navigate between matches
- Press `Enter` to exit search and stay at current match
- Press `Esc` to cancel search and return to original position

### Saving Files

When saving a file for the first time, you'll be prompted to enter a filename. Type the desired name and press `Enter`.

## Configuration

### Tab Size

The default tab stop is 8 spaces. To change this, modify the `KZ_TAB_STOP` constant in `src/main.zig`:

```zig
const KZ_TAB_STOP = 4;  // Change to your preferred tab width
```

### Adding Syntax Highlighting for New Languages

1. Define file extensions and keywords in `src/main.zig`:

```zig
const PYTHON_HL_extensions = [_][]const u8{ ".py", ".pyw" };
const PYTHON_HL_keywords = [_][]const u8{
    "def", "class", "if", "else", "elif", "while", "for",
    "int|", "str|", "bool|", "list|", "dict|",
};
```

2. Add entry to `HLDB` array:

```zig
const HLDB = [_]EditorSyntax{
    // ... existing entries ...
    .{
        .filetype = "python",
        .filematch = &PYTHON_HL_extensions,
        .keywords = &PYTHON_HL_keywords,
        .singleline_comment_start = "#",
        .multiline_comment_start = "\"\"\"",
        .multiline_comment_end = "\"\"\"",
        .flags = HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
};
```

## Development

### Project Structure

```
kz/
├── build.zig          # Build configuration
├── src/
│   └── main.zig       # Main source code
├── LICENSE            # BSD 3-Clause License
└── README.md          # This file
```

### Building for Development

```bash
# Build and run
zig build run

# Build with debug symbols
zig build

# Run tests
zig build test
```

## Troubleshooting

### Terminal Display Issues

If you experience display issues:
- Ensure your terminal supports ANSI escape sequences
- Try resizing your terminal window
- Check that your `TERM` environment variable is set correctly

### Binary Not Found After Installation

If `kz` command is not found:
- Verify the binary location is in your `PATH`
- Reload your shell configuration: `source ~/.bashrc` (or `.zshrc`)
- Check permissions: `ls -l $(which kz)`

### Build Errors

If you encounter build errors:
- Ensure you have the latest Zig version
- Try cleaning the build cache: `rm -rf zig-cache zig-out`
- Rebuild: `zig build`
- Open an Issue on github, Zig is under active development so probably a lot of things are going to break constantly

## License

This project is licensed under the BSD 3-Clause License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## Acknowledgments

Inspired by the [kilo](https://github.com/antirez/kilo) text editor and the excellent [Build Your Own Text Editor](https://viewsourcecode.org/snaptoken/kilo/) tutorial together with [micro](https://github.com/micro-editor/micro) text editor.

Special thanks to [paulsmith](https://github.com/paulsmith), [Ryp](https://github.com/Ryp), [spiral-ladder](https://github.com/spiral-ladder) even thought they will probably never see this project, their own contributions to open source helped me tremendously when learning Zig and writing this project.

## Author

Copyright (c) 2026, Kamil Hajduk aka ky-hy