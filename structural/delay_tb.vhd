-- Greg Stitt
-- University of Florida

-- Basic testbench for delay.vhd. Demonstrates use of random numbers.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity delay_tb is
end delay_tb;


architecture random_tb of delay_tb is

    constant NUM_TESTS : integer := 10000;

    constant CYCLES : natural  := 5;
    constant WIDTH  : positive := 8;

    signal clk_en                        : std_logic := '1';
    signal clk                           : std_logic := '0';
    signal rst, en                       : std_logic;
    signal input, output, output_correct : std_logic_vector(WIDTH-1 downto 0);
    
begin
    
    DUT : entity work.delay
        generic map (
            WIDTH  => WIDTH,
            CYCLES => CYCLES)
        port map (
            clk    => clk,
            rst    => rst,
            en     => en,
            input  => input,
            output => output
            );

    -- Generate a 100 MHz clock. We gate the clock because the simulation
    -- will continue to run as long as there are events to simulate. Without
    -- the gate, this concurrent statement will run forever. With the gate,
    -- we can disable clk_en, and the clock will stop, causing the simulation
    -- to end.
    clk <= not clk and clk_en after 5 ns;

    process
        variable seed1, seed2 : positive := 1;
        variable rand_val     : real;
    begin
        rst   <= '1';
        en    <= '0';
        input <= (others => '0');

        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;

        rst <= '0';
        wait until rising_edge(clk);

        for i in 0 to NUM_TESTS-1 loop
            -- Generating random numbers in VHDL is pretty awkward. We have
            -- to call uniform to get a random real between 0 and 1, and then
            -- manipulate it however we want.
            uniform(seed1, seed2, rand_val);

            -- Generates a ranodom input.
            input                        <= std_logic_vector(to_unsigned(integer(floor(rand_val * real(2**WIDTH-1))), WIDTH));

            -- Generate a random enable.
            uniform(seed1, seed2, rand_val);
            if (rand_val > 0.75) then en <= '1'; else en <= '0'; end if;
            wait until rising_edge(clk);
        end loop;

        -- Disable the clock to terminate the simulation.
        clk_en <= '0';
        report "Tests completed.";
        wait;
    end process;

    -- The output of the delay should match the reference model every cycle.
    assert(not (rising_edge(clk) and output /= output_correct));

    --------------------------------------------------------------------
    -- Reference model
    
    U_CYCLES_GT_0 : if CYCLES > 0 generate

        type reg_array_t is array (0 to CYCLES-1) of std_logic_vector(WIDTH-1 downto 0);
        signal reg_array : reg_array_t;
    begin
        process(clk, rst)
        begin
            if (rst = '1') then
                for i in 0 to CYCLES-1 loop
                    reg_array(i) <= (others => '0');
                end loop;
            elsif (clk'event and clk = '1') then
                if (en = '1') then
                    reg_array(0) <= input;
                end if;

                -- If not needed, but gets rid of null range warning when
                -- CYCLES = 1
                if (CYCLES > 1) then
                    for i in 0 to CYCLES-2 loop
                        if (en = '1') then
                            reg_array(i+1) <= reg_array(i);
                        end if;
                    end loop;
                end if;
            end if;
        end process;

        output_correct <= reg_array(CYCLES-1);

    end generate U_CYCLES_GT_0;

    U_CYCLES_EQ_0 : if CYCLES = 0 generate

        output_correct <= input;

    end generate U_CYCLES_EQ_0;
    
end random_tb;
