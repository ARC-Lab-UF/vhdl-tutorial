# Introduction

This directory provides a tutorial on how to create behavioral descriptions of sequential logic. Technically, sequential logic only includes flip-flops and registers
(possibly latches in rare cases or for ASICs), so the tutorial actually shows how to model circuits that are a combination of both sequential logic and
combinational logic. The ultimate purpose of this tutorial is to understand how registers get synthesized, and where they get placed in relation to other logic. One of the most
common mistakes when writing RTL code is accidentally introducing an incorrect number of registers.

# Methodology: design the circuit, then write the code.

For circuits with sequential logic, designing the circuit means deciding exactly how many registers you want, and what those registers should be connected to. Although synthesis 
optimizations may change this some (e.g. via retiming), use of registers is a critical design decision because it affects the timing of your design, which is something RTL 
synthesis cannot change. Similar to structural architectures, "designing the circuit" for sequential logic usually means creating a schematic that illustrates the exact number
and placement of all registers. With this schematic, you can easily apply the guidelines given below to ensure your design synthesizes as intended.

# Suggested Study Order

1. [Register](reg.vhd)
    - Illustrates how to create an asynchronous reset, a synchronous reset, an enable/load, and a highly parameterized register with different reset types and activiation levels.
1. [Examples of Synthesizing Behavioral Code to a Specific Structure](seq_example.vhd)
    - See [architectures.pdf](architectures.pdf) for different example circuits. Each one has a corresponding module in [seq_example.vhd](seq_example.vhd).
    - Illustrates the important rule that if you assign a signal on a rising clock edge, it becomes a register.
    - Illustrates common mistakes with sequential logic.
    - Goes over the use of signals and variables to accomplish different goals.
    - Suggestion: synthesize each module and use an RTL viewer to ensure the schematic matches the architecture in the pdf. There are no provided testbenches for these examples since they are solely intended to match the structure of the circuits in architectures.pdf. 
1. [Delay](delay.vhd)
    - Illustrates behavioral alternative to earlier structural delay that still synthesizes the same circuit.
    - Illustrates default input values.
    - Illustrates synthesis attributes to prevent inferring of non-flip-flop resources (e.g., embedded RAM).
    - NOTE: This is very useful entity. I use it in almost every application I work on. I've had heavily pipelined applications where delays were the primary resource resource bottleneck.
1. [Counter](counter.vhd)
    - Demonstrates various way of creating counters.
    - Illustrates differences between integer and unsigned/signed types.
    - Explains how to create functions in packages so they can be used in ports.
    - Demonstrates usage of signal attributes to replace constants and ranges.
    - Testbench demonstrates how to simulate for a specific amount of time, and how to verify that various events were tested.
    

