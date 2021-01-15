# CMPUT 382: Intro to GPU Programming

## Introduction

- Latency devices
  - Low latency but low throughput.
  - CPUs
- Throughput (Parallel) devices
  - High latency but high throughput.
  - GPUs, many many CPUs.
- Heterogeneous Parallel Computing
  - Many different types of devices helping compute the same thing, usually implemented in the cloud.
  - Usually, latency cores do management and scheduling of the throughput and heterogeneous devices such as DSPs and FPGAs.

Flynn's Taxonomy:

- SISD: Single Instruction, Single Data
  - Classical Processors
- MISD: Multiple Instruction, Single Data
- SIMD: Single Instruction, Multiple Data
  - GPUs
- MIMD: Multiple Instruction, Multiple Data

A thread is a sequence of **instructions**. Data is irrelevant.

A warp is a set of ALUs controlled by the same control unit with access to the same cache, and each warp executes the same thread.
