# VHDL Tutorial, Greg Stitt

# Introduction

This repository provides a tutorial on how to write synthesizable VHDL code. It touches on verification topics, but the primary focus is on code for synthesis. Most of the provided examples include multiple implementations that illustrate common mistakes, different ways of implementing the same circuit, or different tradeoffs.

# Prerequisites

This tutorial assumes you already have a background in digital logic, synthesis tools, and simulators. All examples have been tested in Quartus and Modelsim, for which there are free versions available for students.

# Methodology: design the circuit, then write the code.

My biggest suggestion for writing synthesizable code in any language is to design the circuit, then write the code. Basically, you should be able to hierarchically divide a large circuit into smaller and smaller components, until each component is either combinational logic, sequential logic, a combination of both (e.g., a datapath), finite state machines, or memories. Then, you can simply follow the following synthesis guidelines for each of these types of circuits. Note that some of the guidelines contradict each other if used for the wrong type of logic, which is why it is always important to know the type of logic that you are designing. In other words, don't start writing the code until you know what type of circuit you are describing. If you are creating a structural archiecture, draw the schematic first. If you are designing a state machine, draw the FSM first. If you are designing a circuit with registers, first figure out exactly where you want registers, etc.

# VHDL vs. Verilog vs. SystemVerilog

I am frequently asked which RTL language is "best," or why I prefer one over another. Personally, if I was forced to pick one language, I would choose SystemVerilog. However, this comes with many disclaimers. Verilog and SystemVerilog (which I'll refer to collectively as Verilog for simplicity) are often preferred due to more convenient syntax than VHDL, and due to Verilog automatically doing certain things for you (e.g., converting widths automatically). However, the convenience of Verilog doing things for you means in many cases it will do things you did not intend. I frequently have mistakes in my Verilog code that I'm shocked actually compile without warnings. In many cases, identifying these problems is a very time consuming process. Even worse, some of these issues can lead to the dreaded situation of a design working in simulation but not after synthesis. Since these types of problems can take an indefinite amount of time to debug, I strongly prefer VHDL as the initial RTL language that people should learn. VHDL will catch the vast majority of these problems at compile time and does not require learning numerous problematic uses of common constructs (e.g. race conditions from blocking assignments). Once a designer has significant experience creating synthesizable code, it then becomes safer to switch to Verilog. I have had many students hired into Verilog jobs only knowing VHDL, and they have excelled.

# Suggested Study Order

1. [Combinational Logic]()
1. [Structural Architectures]()
1. [Sequential Logic]()
1. [Finite-State Machines]()
1. [Finite-State Machines + Datapaths]()
1. [Problematic Coding Practices (Gotchas)]()
1. [Testbenches]()
