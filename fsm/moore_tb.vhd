-- Greg Stitt
-- University of Florida

-- Entity: moore_tb
-- Description: Testbench for the moore module, which implements a Moore FSM.
-- Note that if moore is changed to use the 1-process version, this testbench
-- will start to report errors because of the 1-cycle delay for the outputs.
-- It is left as an exercise to adapt the testbench to work for both models.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity moore_tb is
end moore_tb;

architecture random_tb of moore_tb is

    constant NUM_CYCLES : integer := 10000;

    signal clk_en                        : std_logic := '1';
    signal clk                           : std_logic := '0';
    signal rst, en                       : std_logic;
    signal output, output_correct : std_logic_vector(3 downto 0);
    
begin
    
    DUT : entity work.moore
        port map (
            clk    => clk,
            rst    => rst,
            en     => en,
            output => output
            );

    clk <= not clk and clk_en after 5 ns;

    process
        variable seed1, seed2 : positive := 1;
        variable rand_val     : real;
    begin
        rst   <= '1';
        en    <= '0';
        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;

        rst <= '0';
        wait until rising_edge(clk);

        for i in 0 to NUM_CYCLES-1 loop
            -- Generate a random enable with equal probability of '1' and '0';
            uniform(seed1, seed2, rand_val);
            if (rand_val > 0.5) then en <= '1'; else en <= '0'; end if;
            wait until rising_edge(clk);
        end loop;

        clk_en <= '0';
        report "Tests completed.";
        wait;
    end process;

    -- The output of the register should match the reference model every cycle
    -- that the register isn't being reset.
    assert(not (rising_edge(clk) and rst = '0' and output /= output_correct));

    --------------------------------------------------------------------
    -- Reference model
    --
    -- A testbench for just a register isn't a great example because unless
    -- you have an existing register entity to act as a reference model, you
    -- basically have to use the same sequential code.   
    process(clk, rst)
    begin
        if (rst = '1') then
            output_correct <= "0001";
        elsif (rising_edge(clk)) then
            -- The correct output simply rotates every time the enable is
            -- asserted.
            if (en = '1') then
                output_correct <= output_correct(2 downto 0) & output_correct(3);
            end if;
        end if;
    end process;   
end random_tb;
