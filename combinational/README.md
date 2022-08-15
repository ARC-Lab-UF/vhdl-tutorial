# Introduction

This directory provides a tutorial on how to create synthesizable behavior descriptions of combinational logic. All examples include testbenches for simulation, which uses the name of the module being simulated with a _tb.vhd suffix. Testbenches are commented, but will be explained in detail in a different section of the tutorial. Some of the examples may also have a _tb.sv suffix to demonstrate using SystemVerilog testbenches to test VHDL code.

# Methodology: design the circuit, then write the code.

As with all circuits, first design the combinational circuit, then write the code. With combinational logic, this methodology is often confusing because synthesis tools are generally very good at optimizing combinational logic. So, unlike other types of logic, you can write often combinational logic in many ways that will all synthesize to efficient circuits. However, you should at the very least consider the I/O interface before starting to write the code. You could also try to simplify the logic manually, but for pure combinational logic, synthesis tools will likely do a better job.

# Suggested Study Order

1. [2:1 mux]()
    - Introduces basic constructs and guidelines for cobminational logic. 
    - Includes a top-level module mux2x1 that allows you to change the module that is synthesized.
    - Includes a testbench that tests all included modules at the same time.
1. [4-input Priority Encoder]()
    - Introduces packed arrays.
    - Discusses appropriate situations for if and case statements.
1. [Parameterized Priority Encoder]()
    - Introduces parameters to support any number of inputs.
    - Introduces for loops inside always blocks.
    - Introduces local parameters.
    - Introduces how to convert an integer to any number of bits to avoid width mismatch problems.
1. [Adders]()
    - Introduces arithmetic operations, blocking vs. non-blocking assignments, concatenation, automatic variable resizing.
    - Illustrates a variety of adders (no carry, carry out, carry in & out, carry in, out, and overflow)
1. [Multipliers]()
    - Introduces signed and unsigned, generate statements, variable scope, slicing, and hiearchical access of generate blocks.
    -Testbench tests signed and unsigned instances simultaneously.    
1. [ALU]()
    - Introduces common problems with latches, strategies for avoiding latches, local parameters, and tasks.    
    - Introduces packages, importing, and scope resolution to avoid namespace conflicts.    

