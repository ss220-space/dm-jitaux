FROM llvm-12

RUN apt-get install curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"

RUN rustup toolchain install nightly-2021-11-05-i686-unknown-linux-gnu

ADD auxtools /work/auxtools
ADD dmjit /work/dmjit


WORKDIR /work/dmjit

RUN rustup target add i686-unknown-linux-gnu
RUN rustup override set nightly-2021-11-05-i686-unknown-linux-gnu

RUN cargo search lazy_static

ENV LLVM_SYS_120_PREFIX="/usr/local/"

ENV RUSTFLAGS="-L native=/usr/local/lib"

RUN apt-get install libffi-dev:i386
RUN cargo build --release --target i686-unknown-linux-gnu