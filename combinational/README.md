# Introduction

This directory provides a tutorial on how to create synthesizable behavior descriptions of combinational logic. All examples include testbenches for simulation, which uses the name of the module being simulated with a _tb.vhd suffix. Testbenches are commented, but will be explained in detail in a different section of the tutorial. Some of the examples may also have a _tb.sv suffix to demonstrate using SystemVerilog testbenches to test VHDL code.

# Methodology: design the circuit, then write the code.

As with all circuits, first design the combinational circuit, then write the code. With combinational logic, this methodology is often confusing because synthesis tools are generally very good at optimizing combinational logic. So, unlike other types of logic, you can write often combinational logic in many ways that will all synthesize to efficient circuits. However, you should at the very least consider the I/O interface before starting to write the code. You could also try to simplify the logic manually, but for pure combinational logic, synthesis tools will likely do a better job.

# Coding Guidelines for Combinational Logic

The examples below give many suggestions, but there are two guidelines that should never be violated for combinational logic:
 1. All inputs must be in the sensitivity list for a process, or there likely will be difference between simulation and synthesis.
 1. All outputs must be defined on all paths through a process, or latches will be created during synthesis.

# Suggested Study Order

1. [2:1 mux](./mux_2x1.vhd)
    - Introduces basic constructs and guidelines for combinational logic. 
    - Includes a testbench that tests all included architectures at the same time.
1. [4-input Priority Encoder](./priority_encoder_4in.vhd)
    - Introduces std_logic_vectors.
    - Discusses appropriate situations for if and case statements.
    - Introduces the case? VHDL 2008 construct
1. [Generic Priority Encoder](./priority_encoder.vhd)
    - Introduces generics to support any number of inputs.
    - Introduces for loops inside processes.
    - Introduces constants.
    - Introduces how to convert an integer to any number of bits to avoid width-mismatch problems.
1. [Adders](./add.vhd)
    - Introduces arithmetic operations, signed and unsigned, sign extension, concatenation, type casting.
    - Demonstrates the differences between three different package combinations for arithmetic operations. 
1. [Multipliers](./mult.vhd)
    - Introduces multiplication, if-generate statements, and slicing.
    - Testbench tests signed and unsigned instances simultaneously.
1. [ALU](./alu.vhd)
    - Introduces common problems with latches and strategies for avoiding latches.
    - Introduces constants, enumerated types, encodings of enumerated types, don't cares, procedures, and packages ([alu_pkg](./alu_pkg.vhd))

