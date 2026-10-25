# red4ext-rs-dumper

`red4ext-rs-dumper` is a RED4ext plugin written in Rust that automatically dumps RTTI (Run-Time Type Information) types to Rust definitions. It is built using [red4ext-rs](https://github.com/jac3km4/red4ext-rs).

## Features

- Hooks into the game's initialization process.
- Extracts RTTI system information.
- Automatically formats and dumps game classes, enumerations, and bitfields into a `generated.rs` Rust source file.
- Handles type alignments, padding, offsets, and field naming conventions, adapting native representations for Rust structs.
- Provides test generations for size/alignment verification of the extracted classes.

## Building

This project requires a standard Rust toolchain. You can build it using `cargo`:

```sh
cargo build --release
```

This will produce a dynamic library (e.g., `.dll` on Windows) in the `target/release/` directory.

## Usage

When compiled as a `cdylib` and loaded into the game as a RED4ext plugin, the dumper runs automatically upon entering the `Running` state of the game.

The RTTI types will be processed and output into a `generated.rs` file in the game directory.

## License

This project is open-source. See the `LICENSE` file for details.
