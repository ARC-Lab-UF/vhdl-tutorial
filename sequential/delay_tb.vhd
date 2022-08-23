-- Greg Stitt
-- University of Florida

-- Basic testbench for delay.vhd. Demonstrates use of random numbers.
--
-- NOTE: This is an example of where using a SystemVerilog testbench would
-- be a better idea. This will be illustrated in the testbenches section.

library ieee;
use ieee.std_logic_1164.all;

-- A register to be used by the reference model in the testbench.

entity delay_reg is
    generic(WIDTH : positive;
            RESET_VALUE : std_logic_vector := "");
    port(clk    : in  std_logic;
         rst    : in  std_logic;
         en     : in  std_logic;
         input  : in  std_logic_vector(WIDTH-1 downto 0);
         output : out std_logic_vector(WIDTH-1 downto 0));
end delay_reg;

architecture BHV of delay_reg is
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            if (RESET_VALUE /= "") then
                output <= RESET_VALUE;
            else
                output <= (others => '0');
            end if;
        elsif (rising_edge(clk)) then
            if (en = '1') then
                output <= input;
            end if;
        end if;       
    end process;
end BHV;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- The delay testbench.

entity delay_tb is
end delay_tb;

architecture random_tb of delay_tb is

    constant NUM_TESTS : integer := 10000;
    
    constant CYCLES : natural  := 5;
    constant WIDTH  : positive := 8;
    constant RESET_VALUE : std_logic_vector(WIDTH-1 downto 0) := (others => '1');
    
    signal clk_en                        : std_logic := '1';
    signal clk                           : std_logic := '0';
    signal rst, en                       : std_logic;
    signal input, output, output_correct : std_logic_vector(WIDTH-1 downto 0);
    
begin
    
    DUT : entity work.delay
        generic map (
            WIDTH  => WIDTH,
            CYCLES => CYCLES,
            RESET_VALUE => RESET_VALUE)
        port map (
            clk    => clk,
            rst    => rst,
            en     => en,
            input  => input,
            output => output
            );

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
            uniform(seed1, seed2, rand_val);
            input                        <= std_logic_vector(to_unsigned(integer(floor(rand_val * real(2**WIDTH-1))), WIDTH));

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

    CYCLES_EQ_0 : if (CYCLES = 0) generate    
        output_correct <= input;
    end generate CYCLES_EQ_0;

    CYCLES_GT_0 : if (CYCLES > 0) generate       
        type reg_array_t is array (0 to CYCLES) of std_logic_vector(WIDTH-1 downto 0);
        signal d : reg_array_t;
    begin       
        U_REGS : for i in 0 to CYCLES-1 generate
            U_REG : entity work.delay_reg
                generic map (WIDTH => WIDTH,
                             RESET_VALUE => RESET_VALUE)
                port map (
                    clk    => clk,
                    rst    => rst,
                    en     => en,
                    input  => d(i),
                    output => d(i+1)
                    );
        end generate;

        d(0) <= input;
        output_correct <= d(CYCLES);
    end generate CYCLES_GT_0;
    
end random_tb;
