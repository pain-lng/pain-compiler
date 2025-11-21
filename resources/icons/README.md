# Application Icons

This directory contains application icons for Pain compiler and LSP server.

## Structure

```
icons/
├── linux/          # Source PNG files (16x16 to 512x512)
│   ├── pain_*.png
│   └── lsp_*.png
├── windows/        # Generated ICO files (auto-generated, gitignored)
│   ├── pain.ico
│   └── lsp.ico
└── macOS/          # Generated ICNS files (auto-generated, gitignored)
    ├── pain.icns
    └── lsp.icns
```

## Generating Icons

Run the conversion script from the repository root to regenerate ICO/ICNS files:

```bash
python utils/convert_icons.py
```

**Requirements:**
- Python 3
- Pillow: `pip install Pillow`

**Note:** ICNS files require macOS `iconutil` command. On non-macOS systems, the script will create the iconset structure, but you'll need to run `iconutil` on macOS to create the final ICNS file.

## Usage in Build

- **Windows**: Icons are automatically embedded via `build.rs` using `winres` crate
- **macOS**: Icons can be used with `cargo-bundle` or manual app bundle creation
- **Linux**: PNG files are used directly by desktop environments

## Gitignore

Generated ICO and ICNS files are gitignored. Only source PNG files are committed.

