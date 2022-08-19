-- Greg Stitt
-- University of Florida
--
-- This package defines constants and attributes that assist the ALU entities
-- in alu.vhd.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package alu_pkg is
    -- Create an enumerated type for the ALU select so that we can see
    -- the names in the simulation instead of binary values. The _SEL suffix
    -- isn't really necessary because custom types have their own namespace,
    -- but in this case and and or would conflict with VHDL keywords.
    type alu_sel_t is (ADD_SEL, SUB_SEL, AND_SEL, OR_SEL);

    -- enum_encoding is a synthesis attribute defined by the IEEE standard
    -- to control the encoding of enumerated types. You only need this if you
    -- want the enumeration to use specific values. In many cases, it is best
    -- to leave these values to the synthesis tool to increase optimization
    -- potential.
    --
    -- WARNINGS: Use this with caution. Although a synthesis tool is supposed
    -- to use this encoding, there is a decent chance some tools will ignore it.
    -- If you have logic that is based on a specific encoding, it might not
    -- work in all tools. In addition, it will very likely result in differences
    -- between simulation and synthesis because the simulator will not use this
    -- encoding.
    --
    -- SUGGESTION: If you are going to use an enumerated type, either:
    -- 1) Don't specify the encoding, or
    -- 2) Specify the encoding, but don't have any logic that requires that
    --    encoding for correct functionality. e.g. don't compare with a
    --    specific encoding, and don't assign a specific encoding to a signal.
    --    Only use the enumerated type.
    attribute enum_encoding : string;
    attribute enum_encoding of alu_sel_t : type is "00 01 10 11";

    -- If you need to access the std_logic_vector encoding in code, you can use
    -- the following attribute:
    
    attribute encoding : std_logic_vector;    
    attribute encoding of ADD_SEL : literal is "00";
    attribute encoding of SUB_SEL : literal is "01";
    attribute encoding of AND_SEL : literal is "10";
    attribute encoding of OR_SEL : literal is "11";

    -- For example, you could do something like this:
    --
    -- signal sel : std_logic_vector(1 downto 0);
    -- ...
    -- sel <= ADD_SEL'encoding;
    --
    -- Unfortunately, I don't know of an elegant way to directly access the
    -- enum_encoding, so be extra careful to ensure that the
    -- encodings in the custom attribute match the encodings in enum_encoding.
    --
    -- This code also comes with the annoyance that synthesis tools will report
    -- this as a unrecognized syntheis attribute warning. Unfortunately VHDL
    -- does not have a way of distinguishing between synthesis attributes
    -- and custom attributes.

    -- IMPORTANT: although this encoding attribute gives you a way of assigning
    -- alu_sel_t to a std_logic_vector, I do not know of a way of assigning a
    -- std_logic_vector to alu_sel_t. If required, you would probably have to do
    -- something like this:
    --
    -- sel <= ADD_SEL when val = ADD_SEL'encoding else
    --        SUB_SEL when val = SUB_SEL'encoding else
    -- etc.
    --
    -- Again, use this strategy at your own risk because synthesis tools can
    -- ignore the requested encoding.
        
end package;
