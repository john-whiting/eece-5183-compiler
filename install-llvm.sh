#!/usr/bin/env bash

sudo cat <<EOF >> /etc/apt/sources.list
# LLVM 17
deb http://apt.llvm.org/lunar/ llvm-toolchain-lunar-17 main
deb-src http://apt.llvm.org/lunar/ llvm-toolchain-lunar-17 main
EOF

sudo apt-get update


# Install every "key" package for llvm

# LLVM
sudo apt-get install -y libllvm-17-ocaml-dev libllvm17 llvm-17 llvm-17-dev llvm-17-doc llvm-17-examples llvm-17-runtime
# Clang and co
sudo apt-get install -y clang-17 clang-tools-17 clang-17-doc libclang-common-17-dev libclang-17-dev libclang1-17 clang-format-17 python3-clang-17 clangd-17 clang-tidy-17
# compiler-rt
sudo apt-get install -y libclang-rt-17-dev
# polly
sudo apt-get install -y libpolly-17-dev
# libfuzzer
sudo apt-get install -y libfuzzer-17-dev
# lldb
sudo apt-get install -y lldb-17
# lld (linker)
sudo apt-get install -y lld-17
# libc++
sudo apt-get install -y libc++-17-dev libc++abi-17-dev
# OpenMP
sudo apt-get install -y libomp-17-dev
# libclc
sudo apt-get install -y libclc-17-dev
# libunwind
sudo apt-get install -y libunwind-17-dev
# mlir
sudo apt-get install -y libmlir-17-dev mlir-17-tools
# bolt
sudo apt-get install -y libbolt-17-dev bolt-17
# flang
sudo apt-get install -y flang-17
# wasm support
sudo apt-get install -y libclang-rt-17-dev-wasm32 libclang-rt-17-dev-wasm64 libc++-17-dev-wasm32 libc++abi-17-dev-wasm32 libclang-rt-17-dev-wasm32 libclang-rt-17-dev-wasm64 

# Add llvm-config alternative
sudo update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-17 17
