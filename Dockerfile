FROM ghcr.io/ricosjp/monolish/mkl:0.14.2
RUN apt-get update && apt-get install -y \
    gnupg \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    netbase \
    xz-utils \
    zlib1g-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
