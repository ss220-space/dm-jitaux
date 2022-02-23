ARG REPOSITORY_NAME
FROM $REPOSITORY_NAME/dmjit-llvm-linux:main


RUN apt-get install curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"

RUN rustup toolchain install nightly-2021-11-05-i686-unknown-linux-gnu
RUN rustup target add i686-unknown-linux-gnu
RUN rustup override set nightly-2021-11-05-i686-unknown-linux-gnu

RUN cargo search lazy_static

ENV LLVM_SYS_120_PREFIX="/usr/local/"
ENV RUSTFLAGS="-L native=/usr/local/lib"

RUN apt-get install libffi-dev:i386