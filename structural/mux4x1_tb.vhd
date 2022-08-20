-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux4x1_tb is
end mux4x1_tb;


architecture default_tb of mux4x1_tb is

    signal inputs : std_logic_vector(3 downto 0);
    signal sel    : std_logic_vector(1 downto 0);
    signal output : std_logic;

begin  -- TB

    UUT : entity work.mux4x1
        port map (
            inputs => inputs,
            sel    => sel,
            output => output);

    process
        variable temp : std_logic_vector(2 downto 0);

    begin

        for i in 0 to (2**inputs'length)-1 loop
            inputs <= std_logic_vector(to_unsigned(i, inputs'length));

            for j in 0 to (2**sel'length)-1 loop
                sel <= std_logic_vector(to_unsigned(j, sel'length));
                wait for 10 ns;
                assert(output = inputs(j));
            end loop;
        end loop;

        report "Tests completed.";
        wait;

    end process;

end default_tb;
