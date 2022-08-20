-- Greg Stitt
-- University of Florida

-- This example demonstrates how to structurally create a delay by connecting
-- a series of registers. It introduces arrays and if-generate statements.
--
-- See delay.pdf for the schematic that represents the structural architecture.

library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg
-- Description: A basic register entity. If you haven't read the sequential
-- logic section of the tutorial, just skip over the register and read the
-- delay code.

entity reg is
    generic(WIDTH : positive);
    port(clk    : in  std_logic;
         rst    : in  std_logic;
         en     : in  std_logic;
         input  : in  std_logic_vector(WIDTH-1 downto 0);
         output : out std_logic_vector(WIDTH-1 downto 0));
end reg;

architecture BHV of reg is
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            output <= (others => '0');
        elsif (rising_edge(clk)) then
            if (en = '1') then
                output <= input;
            end if;
        end if;       
    end process;
end BHV;


library ieee;
use ieee.std_logic_1164.all;

-- Entity: delay
-- Description: delays the WIDTH-bit input signal by CYCLES cycles. Stalls when
-- en is not asserted. All signals are active high.
--
-- It might seem weird to use a natural for the CYCLES generic instead of a
-- positive, which includes the value 0. You would never intentionally
-- instantiate a delay with 0 cycles, but it can surprisingly occur a lot
-- when the delay is instantiated within other parameterized code.

entity delay is
    generic(CYCLES : natural := 8;
            WIDTH  : positive := 16);
    port(clk    : in  std_logic;
         rst    : in  std_logic;
         en     : in  std_logic;
         input  : in  std_logic_vector(WIDTH-1 downto 0);
         output : out std_logic_vector(WIDTH-1 downto 0));
end delay;


architecture STR of delay is
begin
    -- What does it mean to delay a signal by 0 cycles? Basically, it means
    -- we just want a wire. As a result, we end up with an exception to the
    -- structure, where we just want a wire when CYCLES == 0, and a series of
    -- registers when CYCLES > 0.
    --
    -- To support structural exceptions, VHDL provides the if-generate
    -- construct.
    
    CYCLES_EQ_0 : if (CYCLES = 0) generate
        -- Create a wire when CYCLES == 0.
        output <= input;
    end generate CYCLES_EQ_0;

    -- Unfortunately, VHDL pre-2008 does not have an else generate, so we need
    -- make the else condition explicit.
    CYCLES_GT_0 : if (CYCLES > 0) generate

        -- We created a chain of signals in the ripple carry adder, but in that
        -- example, each element of the chain was a single bit. For the delay,
        -- each element is WIDTH bits. What we ultimately want is an array
        -- where each element is WIDTH bits, which is what we declare here.
        -- This particular array type is contrained, meaning that we specify the
        -- range of the array in the type declaration. We'll see unconstrained
        -- arrays in later examples.
        --
        -- Note that we declared the type and signal within the generate.
        -- If we had declared them for the entire architecture, it would still
        -- work, but we would get synthesis warnings about unused signals
        -- in the case where CYCLES == 0.
        
        type reg_array_t is array (0 to CYCLES) of std_logic_vector(WIDTH-1 downto 0);

        -- We now create an instance of the array.
        signal d : reg_array_t;
    begin
        
        -- Use a for-generate to create a series of registers.
        U_REGS : for i in 0 to CYCLES-1 generate
            U_REG : entity work.reg
                generic map (WIDTH => WIDTH)
                port map (
                    clk    => clk,
                    rst    => rst,
                    en     => en,
                    input  => d(i),
                    output => d(i+1)
                    );
        end generate;

        -- Connect the delay's input and output.
        d(0) <= input;
        output <= d(CYCLES);
    end generate CYCLES_GT_0;
    
end STR;
