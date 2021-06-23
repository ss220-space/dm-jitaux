![WIP](https://img.shields.io/badge/-WIP-yellow) 
![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)
# dm-jitaux

dm-jitaux is a Rust-based JIT compiler using modified [auxtools](https://github.com/ss220-space/auxtools), [dmasm](https://github.com/ss220-space/dmasm) and [Inkwell](https://github.com/TheDan64/inkwell) LLVM wrapper for boosting [Byond DM](http://www.byond.com) performance without any hassle (such as rewriting/refactroing your DM code).

## Implementation status
|OpCode|Status| 
|------|------|
|GetVar||
|SetVar||
|Add||
|Ret||
|End||
|Test ||
|Jz||
|PushInt||

## Dependencies
*These instructions were taken directly from tgstation's [rust-g](https://github.com/tgstation/rust-g) documentation.*

The [Rust] compiler:

1. Install the Rust compiler's dependencies (primarily the system linker):

    * Ubuntu: `sudo apt-get install gcc-multilib`
    * Windows (MSVC): [Build Tools for Visual Studio 2017][msvc]
    * Windows (GNU): No action required

1. Use [the Rust installer](https://rustup.rs/), or another Rust installation method,
   or run the following:

    ```sh
    curl https://sh.rustup.rs -sSfo rustup-init.sh
    chmod +x rustup-init.sh
    # Nightly toolchain is required
    ./rustup-init.sh
    ```
    Alternatively without saving
    ```sh
    # Nightly toolchain is required
    curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly
    ```

1. Set the default compiler to **32-bit**:

    ```sh
    # Clone the `dm-jitaux` repository to a directory of your choice
    git clone https://github.com/ss220-space/dm-jitaux.git
    # in the `dm-jitaux` directory...
    cd dm-jitaux
    # Linux
    rustup target add i686-unknown-linux-gnu
    # Windows
    rustup target add i686-pc-windows-msvc
   
    # Clone the `auxtools` repository to a parent directory of dm-jitaux
    cd ..
    git clone https://github.com/ss220-space/auxtools.git --branch dm-jitaux
    # in the `auxtools` directory...
    cd auxtools
    # Linux
    rustup target add i686-unknown-linux-gnu
    # Windows
    rustup target add i686-pc-windows-msvc
    ```

System libraries:

* Ubuntu and Debian users run:

    ```sh
    sudo dpkg --add-architecture i386
    sudo apt-get update
    sudo apt-get install zlib1g-dev:i386 libssl-dev:i386 pkg-config:i386
    sudo apt-get install gcc-multilib g++-multilib
    ```

* Other Linux distributions install the appropriate **32-bit development** and **32-bit runtime** packages.

## Building
### LLVM
#### Windows
CMake required, can be downloaded from [cmake.org](https://cmake.org/download/)

You need to build LLVM 12.0.0 on Windows with x32 target and MD CRT linking

*Note: It takes 30-60 minutes to compile, 23 GB of free disk space*

Download from [LLVM github releases](https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.0/llvm-12.0.0.src.tar.xz)
```sh
# Unpack to path without spaces 
tar -xJf llvm-12.0.0.src.tar.xz
# Create build directory
mkdir build
cd build
# Configure 
cmake -DCMAKE_INSTALL_PREFIX=C:/LLVM -DLLVM_USE_CRT_DEBUG=MD -DLLVM_TARGETS_TO_BUILD="X86" -Thost=x86 -A Win32 ../llvm-12.0.0.src/
# Build
cmake --build .
# Install to CMAKE_INSTALL_PREFIX
cmake --build . --target install
```

Full guide: [Building LLVM with CMake](https://llvm.org/docs/CMake.html) 

#### Debian
```sh
sudo apt install llvm-12-dev:i386
```
#### Other
[Checkout this](https://apt.llvm.org/) for more info.

### Compiling

#### Windows
```sh
# PowerShell
$env:LLVM_SYS_120_PREFIX = "C:\LLVM" # LLVM prefix specified before
cargo build --target i686-pc-windows-msvc
cargo test --target i686-pc-windows-msvc -- --test-threads=1
```
#### Linux
```sh
cargo build --target i686-unknown-linux-gnu
BYOND_PATH="~/byond" cargo test --target i686-unknown-linux-gnu -- --test-threads=1
```

## See also
### DM Opcode relative frequency table
[opcode_counts.txt](opcode_counts.txt)

## License

dm-jitaux is licensed under the  [AGPL-3.0 License](https://ru.wikipedia.org/wiki/GNU_Affero_General_Public_License). See [LICENSE](https://github.com/ss220-space/dm-jitaux/blob/main/LICENSE)  for more details.

[Rust]: https://rust-lang.org
[msvc]: https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools&rel=15
