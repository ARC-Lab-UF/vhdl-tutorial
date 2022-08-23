-- Greg Stitt
-- University of Florida

-- Basic testbench for the counter entities. Demonstrates how to simulate
-- for a specific amount of time.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.counter_pkg.all;

entity counter_tb is
end counter_tb;


architecture TB of counter_tb is

    constant TOTAL_TIME  : time := 1 ms;
    constant TOGGLE_TIME : time := 500 ns;

    -- INSTRUCTIONS: Change this value to test different counters, but make
    -- sure to set to 15 if testing the 4-bit counter.
    constant MAX_VALUE : integer := 15;
    constant NUM_BITS  : integer := clog2(MAX_VALUE+1);

    signal clk    : std_logic := '0';
    signal rst    : std_logic := '0';
    signal up     : std_logic := '1';
    signal up_val : std_logic := '1';
    signal output : std_logic_vector(NUM_BITS-1 downto 0);

    signal done   : std_logic := '0';
    signal clk_en : std_logic := '1';

    signal overflow  : boolean := false;
    signal underflow : boolean := false;

    -- Reference model signals.
    signal count_r        : unsigned(output'range);
    signal output_correct : std_logic_vector(NUM_BITS-1 downto 0);
    
begin

    -- Change the architecture to test the different implementations and
    -- different max_values
    DUT : entity work.counter
        generic map (MAX_VALUE => MAX_VALUE)
        port map (
            clk    => clk,
            rst    => rst,
            up     => up,
            output => output);

    clk <= not clk and clk_en after 10 ns;

    -- Toggle the more likely value of up every TOGGLE_TIME
    up_val <= not up_val and not done after TOGGLE_TIME;

    -- stop the simulation after 5000 ns;
    done <= '1' after TOTAL_TIME;

    process
        variable seed1, seed2 : positive := 1;
        variable rand_val     : real;

        variable up_temp : std_logic;
    begin
        -- Reset the counter for 4 cycles.
        rst <= '1';
        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;

        -- Clear reset.
        rst <= '0';
        wait until rising_edge(clk);

        while (done = '0') loop
            uniform(seed1, seed2, rand_val);
            if (rand_val > 0.75) then up_temp := up_val; else up_temp := not up_val; end if;
            up                                <= up_temp;

            -- Check to see if overflow or underflow was tested.
            if (unsigned(output) = MAX_VALUE and up_temp = '1') then overflow    <= true; end if;
            if (output = (output'range => '0') and up_temp = '0') then underflow <= true; end if;
            wait until rising_edge(clk);

        end loop;

        clk_en <= '0';

        -- Print warnings if overflow and underflow were tested.
        --
        -- SYSTEMVERILOG COMPARISON: SystemVerilog has a cover property
        -- construct, which is a much more concise way of determining whether
        -- or not various events occurred during a simulation.
        
        assert(overflow) report "WARNING: Overflow never tested.";
        assert(underflow) report "WARNING: Underflow never tested.";
        report "Tests completed.";
        wait;
        
    end process;

    assert(not (rising_edge(clk) and output /= output_correct));

    ---------------------------------------------------------------------
    -- Reference model.

    -- It is a bad idea to reuse one of the implementations as the reference
    -- model, unless that model has already been verified. It is reused here
    -- to avoid creating having to create another counter.

    process(clk, rst)
    begin
        if (rst = '1') then
            count_r <= (others => '0');
            
        elsif (rising_edge(clk)) then
            if (up = '1') then
                if (count_r = MAX_VALUE) then
                    count_r <= to_unsigned(0, count_r'length);
                else
                    count_r <= count_r + 1;
                end if;
            else
                if (count_r = 0) then
                    count_r <= to_unsigned(MAX_VALUE, count_r'length);
                else
                    count_r <= count_r - 1;
                end if;
            end if;
        end if;
    end process;

    output_correct <= std_logic_vector(count_r);
    
end TB;
