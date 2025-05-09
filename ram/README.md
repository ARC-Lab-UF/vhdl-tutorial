# Summary

This directory contains a number of simple dual-port (SDP) and true dual-port (TDP) inference templates
that work on most FPGAs from Xilinx/AMD and Intel/Altera.

# Suggested Study Order

1. [ram_sdp](ram_sdp.vhd)    
    - Illustrates a variety of single dual-port inference templates with different features.
    - Demonstrates a generalized, configurable template that combines these features.    
    - Includes a SystemVerilog [testbench](ram_sdp_tb.sv) for the generalized template.

1. [ram_sdp_with_reset](ram_sdp_with_reset.vhd)    
    - Illustrates how to modify the general template with a reset for some of the optional registers.
    - Explains the risks of using resets improperly in RAM templates.

1. [ram_tdp](ram_tdp.vhd)    
    - Explains a generalized true dual-port RAM inference template.
