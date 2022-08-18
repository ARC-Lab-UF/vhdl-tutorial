-- Greg Stitt
-- University of Florida

-- You can ignore these for now. They will be explained later. For now, think
-- of them as like header files in C++ that define types, functions, etc.
library ieee;
use ieee.std_logic_1164.all;

-- The entity is the most basic construct in VHDL. An entity encapsulates
-- an arbitrary amount of logic, while defining the interface to that logic.

entity mux_2x1 is
    -- The port statement defines the interface to the entity.
    -- std_logic is one of the basic types of VHDL. It defines values such as
    -- '0', '1', '-' (Don't care), 'X' (unknown), 'U' (uninitialized),
    -- 'Z' (high impedance), among others.
    --
    -- One unique difference from Verilog/SV is that Verilog does not have
    -- separate values for don't care and unitialized. Verilog typically uses
    -- 4-state types that define 0, 1, unknown, and high-impedence values.
    -- In Verilog, the unknown value is used for multiple purposes, such as
    -- don't care values, which in my opinion is not ideal. In addition,
    -- I find that separating between uninitialized and unknown is very useful
    -- in simulations. For example, if I don't reset a register, it shows up
    -- as uninitialized, which might be intentional. In Verilog, it shows up as
    -- unknown. The disadvantage of not separating these meanings is that in
    -- VHDL, I can easily find problematic behaviors by looking for Xs. In
    -- Verilog, many of the X values will be acceptable, which makes it harder
    -- to identify problematic instances.
    --
    -- Each input and output on the port is a "signal," which will be explained
    -- in detail later. For now, think of it as a variable from other languages.
    -- However, be aware that VHDL also has variables that behave differently
    -- than signals.

    port(
        in0    : in  std_logic;
        in1    : in  std_logic;
        sel    : in  std_logic;
        output : out std_logic);
end mux_2x1;


-- Now that we have an entity for a circuit, we need to define what that
-- circuit does. In VHDL we do this with an "architecture." One big difference
-- between Verilog and VHDL is that VHDL supports multiple architectures for
-- the same entity. This flexbility can be useful since many circuits naturally
-- have different implementations. For example, a multiplier entity might have a
-- signed and unsigned architecture. Or, another entity might have a low-power
-- architecture and a high-performance architecture. You can accomplish the
-- same things in Verilog, but you either need multiple modules
-- (which replicate the common interface), or you need to use parameters and
-- generate statements to produce different circuits within the same module.

-- In VHDL, there are two primary styles of architectures: behavioral and
-- structural. For now we will focus on behavioral architectures, where we
-- define the behavior of the circuit, and let synthesis create the actual
-- structure from that behavior.

-----------------------------------------------------------------------
-- Mux architectures using sequential statements.
-----------------------------------------------------------------------

-- Sequential statements are similar to statements in commonly used
-- imperative programming languages (C++, Python, etc.).
-- In VHDL, sequential statements can only be used within a process.
-- The process itself is a concurrent statement (which will be explained
-- later), but all the statements inside the process are sequential.
--
-- In simulation, a process executes any time one of the signals in the
-- "sensitivity list" changes, which is the list of signals within parentheses
-- at the top of the process.
--
-- One very important point to understand is that sequential statements do
-- not necessarily synthesize to sequential logic. Also, the order of the
-- statements is not necessarily preserved by the resulting hardware. Synthesis
-- simply replaces the sequentially defined behavior with a circuit that
-- produces the same outputs. The circuit will also produce the output in the
-- same cycle as the code.

architecture if_statement of mux_2x1 is
begin

    -- *********************************************************************
    -- Synthesis guideline for combinational logic: All inputs to the
    -- combinational logic must be included in the sensitivity list.
    -- *********************************************************************
    --
    -- DON'T FORGET "SEL". Leaving an input out of the sensitivity list is a
    -- very common source of bugs. To see what happens, remove "sel" and run the
    -- provided testbench.
    --
    -- VHDL 2008 addresses this problem with the following extension:
    --    process(all)
    -- which is equivalent to SystemVerilog's
    --    always @(*)
    -- or
    --    always_comb

    process(in0, in1, sel)
    begin
        -- Compare the select value with a single 0 bit ('0')
        --
        -- Note that you cannot do:
        -- if (sel) then
        -- This is because VHDL is a strongly typed language and will not
        -- convert types for you. The VHDL grammar requires the if condition
        -- to be of type boolean, which despite being very similar to
        -- std_logic, is technically not the same. As a result, you need the
        -- = operator which returns a boolean. Note that VHDL does *not* use
        -- a double equal (==) for a comparison.
        --
        -- In VHDL 2008 and Verilog, this conversion is done automatically.
        if (sel = '0') then
            -- Signal assignments are done with the <= operator. 
            output <= in0;
        else
            output <= in1;
        end if;
    end process;
end if_statement;


-- In this architecture, we define the behavior similarly using a case statement
-- instead of an in statement. Like the if, the case is also a sequential
-- statement and must therefore be inside a process/

architecture case_statement of mux_2x1 is
begin

    -- Same guideline as before, make sure all inputs are in the sensitivy list
    process(in0, in1, sel)
    begin
        -- Case statement is similar to the if, but only one case can be true.
        case sel is
            when '0' =>
                output <= in0;
            when others =>
                output <= in1;
        end case;

-- I sometimes do this when I want to catch a non '0' or '1' value in
-- simulation. The "null" for the others clause just specifies
-- that nothing should be done if sel isn't '0' or '1'. Synthesis ignores this
-- when others because only the '0' and '1' have meaning in a real circuit.
-- case sel is
-- when '0' =>
-- output <= in0;
-- when '1' =>
-- output <= in1;
-- when others =>
-- null;
-- end case;
    end process;
end case_statement;



-----------------------------------------------------------------------
-- Mux architectures using concurrent statements.
-----------------------------------------------------------------------

-- Unlike sequential statements, concurrent statements all execute at the
-- same time instead of in order. In VHDL, anything outside of a process is
-- a concurrent statement. Any concurrent statement will update its assigned
-- signal when any of its input signals change.
--
-- One common cause of confusion is that the process is actually a concurrent
-- statement. So, all processes, and all statements outside of processes run
-- at the same time. 

-- Here we use a when-else statement to define the mux.

architecture when_else of mux_2x1 is
begin
    -- The when-else is a concurrent equivalent to the sequential if statement.
    output <= in0 when sel = '0' else in1;

    -- Like before, you have also done something like this for simulation
    -- purposes. However, I don't recommend this unless you have a good reason. 
--    output <= in0 when sel = '0' else
--              in1 when sel = '1' else
--              'X';

    -- Important thing to remember: make sure to include an else at the end, or
    -- alternatively you must specify an assignment for each possible input
    -- value.
    
end when_else;



-- Here we define the mux using the with-select construct.
-- NOTE: I have experienced problems with this construct in Vivado's simulator
-- and suggest avoiding it for this reason. I have not had problems in any
-- other simulators.

architecture with_select of mux_2x1 is
begin
    -- The with-select is the concurrent equavilent to the case statement.
    -- Like the case statement, only one of the conditions can be true.
    with sel select
        output <= in0 when '0',
        in1           when others;

    -- For simulation, you could do something similar as the earlier examples
    -- and have a separate when others. Again, I don't recommend this unless you
    -- have a good reason, but synthesis will create the same circuit.
--  with sel select
--    output <= in0 when '0',
--    in1           when '1',
--    'X'           when others;

    -- Important thing to remember: make sure to include "others", or
    -- alternatively you must specify a when clause for each possible value.

end with_select;


-- This architecture would never be needed in normal situations. I use it
-- here to provide a convenient way of synthesizing or simulating different
-- architectures. Simply change the name of the architecture in parentheses
-- after work.mux_2x1 to evaluate each architecture.

architecture default_arch of mux_2x1 is
begin

    -- INSTRUCTIONS: Uncommend the line with the architecture you want to
    -- synthesize or simulate.
    UUT : entity work.mux_2x1(if_statement)
    --UUT : entity work.mux_2x1(case_statement)
    --UUT : entity work.mux_2x1(when_else)
    --UUT : entity work.mux_2x1(with_select)
        port map (in0 => in0,
                  in1 => in1,
                  sel => sel,
                  output => output);
    
end default_arch;
