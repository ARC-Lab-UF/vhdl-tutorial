-- Greg Stitt
-- University of Florida

-- Entity: mealy_tb
-- Description: Testbench for the mealy module.
-- NOTE: This testbench does not verify all outputs and only provides an input
-- stimulus. It only checks if done is asserted.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity mealy_tb is
end mealy_tb;

architecture random_tb of mealy_tb is

    signal clk_en                 : std_logic := '1';
    signal clk                    : std_logic := '0';
    signal rst, en, ack, go, done : std_logic;
    
begin
    
    DUT : entity work.mealy
        port map (
            clk  => clk,
            rst  => rst,
            go   => go,
            ack  => ack,
            en   => en,
            done => done
            );

    clk <= not clk and clk_en after 5 ns;

    process
    begin
        rst <= '1';        
        ack  <= '0';
        go <= '0';
        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;

        rst <= '0';
        wait until rising_edge(clk);

        go <= '1';
        for i in 0 to 4 loop
            wait until rising_edge(clk);
        end loop;

        ack <= '1';
        wait until rising_edge(clk);
        ack <= '0';
        assert(done = '1') report "Done not asserted.";

        go <= '0';
        wait until rising_edge(clk);

        go <= '1';
        for i in 0 to 4 loop
            wait until rising_edge(clk);
        end loop;

        ack <= '1';
        wait until rising_edge(clk);
        ack <= '0';
        assert(done = '1') report "Done not asserted after restart.";
        go <= '0';
        
        clk_en <= '0';
        report "Tests completed.";
        wait;
    end process;
end random_tb;
