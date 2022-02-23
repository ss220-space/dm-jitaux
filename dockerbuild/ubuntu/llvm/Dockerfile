FROM ubuntu:20.04

RUN dpkg --add-architecture i386
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -yy install \
        software-properties-common \
        build-essential \
        gcc-multilib \
        g++-multilib \
        git \
        wget \
        autoconf \
        pkg-config \
        m4 \
        python-dev:i386 \
        libcurl4-gnutls-dev:i386 \
        libncurses-dev:i386 \
        uuid-dev:i386 \
        libx11-dev:i386 \
        libxext-dev:i386 \
        libtinfo-dev:i386 \
        libedit-dev:i386 \
        swig \
        libedit-dev \
        python-dev \
        cmake g++ gcc python3 \
        libncurses5-dev:i386 \
        libncurses5:i386 \
        libtinfo-dev:i386

RUN mkdir -p /work/build
RUN mkdir -p /work/llvm-src
WORKDIR /work/llvm-src
ADD https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.1/llvm-13.0.1.src.tar.xz llvm-13.0.1.src.tar.xz
RUN tar -xvf llvm-13.0.1.src.tar.xz

WORKDIR /work/build
RUN cmake -DCMAKE_LIBRARY_PATH="/usr/lib/i386-linux-gnu" -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="X86" -DLLVM_BUILD_32_BITS=ON -DLLVM_BUILD_TESTS=OFF -DLLVM_BUILD_BENCHMARKS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF ../llvm-src/llvm-13.0.1.src
RUN cmake --build . -j 16
RUN cmake --install .