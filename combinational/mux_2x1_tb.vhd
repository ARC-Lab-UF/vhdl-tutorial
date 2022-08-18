-- Greg Stitt
-- University of Florida

-- Description: This module illustrates a very basic testbench for the
-- mux_x2x1 entity. Testbenches will be explained in more detail later.
--
-- Unlike most testbenches, this testbench tests all of the different
-- architectures of the entity. It is more common to test a single architecture
-- that is either hardcoded, specified via a generic/parameter, or specified
-- using the configuration keyword.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The testbench is just an entity with no I/O. Testbenches can have I/O, but
-- they usually don't unless you are creating a hierarchy of testbenches.
entity mux_2x1_tb is
end mux_2x1_tb;


architecture default_tb of mux_2x1_tb is

    -- Declare local signals for all I/O of the entity we want to test.
    -- I highly suggest using the same names as the entity's port.
    signal in0                : std_logic;
    signal in1                : std_logic;
    signal sel                : std_logic;
    signal output_with_select : std_logic;
    signal output_when_else   : std_logic;
    signal output_if          : std_logic;
    signal output_case        : std_logic;

begin  -- TB

    -- Create an instance of each architecture, which is specified in
    -- parentheses. Entity instantiations will be explained more in the
    -- structural architecture section.
    U_WITH_SELECT : entity work.mux_2x1(with_select)
        port map (
            in0    => in0,
            in1    => in1,
            sel    => sel,
            output => output_with_select);

    U_WHEN_ELSE : entity work.mux_2x1(when_else)
        port map (
            in0    => in0,
            in1    => in1,
            sel    => sel,
            output => output_when_else);

    U_IF : entity work.mux_2x1(if_statement)
        port map (
            in0    => in0,
            in1    => in1,
            sel    => sel,
            output => output_if);

    U_CASE : entity work.mux_2x1(case_statement)
        port map (
            in0    => in0,
            in1    => in1,
            sel    => sel,
            output => output_case);

    -- Use a process to drive inputs of the entities. A process without
    -- a sensitivity list is essentially an infinite loop that starts at the
    -- beginning of the simulation.
    process
        -- Variables will be explained in later examples, but the key difference
        -- from a signal is they are updated immediately, and have a scope that
        -- is limited to the process.
        variable temp : std_logic_vector(2 downto 0);

        -- Define a function to act as a reference model for correct outputs.
        function mux_test (
            signal in0 : std_logic;
            signal in1 : std_logic;
            signal sel : std_logic)
            return std_logic is
        begin
            if (sel = '0') then
                return in0;
            else
                return in1;
            end if;
        end mux_test;

    begin
        -- There are only 8 input combinations for a 2:1 mux, so test them all.
        for i in 0 to 7 loop
            -- Put the loop index into the 3-bit variable.
            temp := std_logic_vector(to_unsigned(i, 3));
            
            -- Slice into the variable to get bits for the inputs.
            in0  <= temp(2);
            in1  <= temp(1);
            sel  <= temp(0);
            wait for 10 ns;

            -- Verify the outputs.
            -- This shows that string operations are pretty inconvenient in
            -- VHDL, which is where Verilog/SV has clear advantages.
            assert(output_with_select = mux_test(in0, in1, sel))
                report "Error : output_with_select incorrect for in0 = " & std_logic'image(in0) & " in1 = " & std_logic'image(in1) & " sel = " & std_logic'image(sel) severity warning;

            assert(output_when_else = mux_test(in0, in1, sel))
                report "Error : output_when_else incorrect for in0 = " & std_logic'image(in0) & " in1 = " & std_logic'image(in1) & " sel = " & std_logic'image(sel) severity warning;

            assert(output_if = mux_test(in0, in1, sel))
                report "Error : output_if incorrect for in0 = " & std_logic'image(in0) & " in1 = " & std_logic'image(in1) & " sel = " & std_logic'image(sel) severity warning;

            assert(output_case = mux_test(in0, in1, sel))
                report "Error : output_case incorrect for in0 = " & std_logic'image(in0) & " in1 = " & std_logic'image(in1) & " sel = " & std_logic'image(sel) severity warning;

        end loop;  -- i

        report "Simulation complete!!!";

        -- The wait statement ends the infinite loop created by the process.
        wait;

    end process;

end default_tb;
