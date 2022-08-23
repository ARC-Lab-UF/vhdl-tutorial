-- Greg Stitt
-- University of Florida
--
-- Entity: delay
-- Description: This entity delays an input by a specified number of cycles,
-- while also allowing stalls and specific output values on reset.
--
-- This design synthesizes to the same structure as the delay in the
-- structural architectures section. See the delay.pdf schematic from that
-- section for a reference.
--

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------
-- Generic Descriptions
-- CYCLES : The length of the delay in cycles (required)
-- WIDTH  : The WIDTH of the input signal (required)
-- RESET_VALUE : An initial value (of WIDTH bits) for the first "CYCLES" outputs
--               after a reset. (optional)
-------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Port Description
-- clk : clock
-- rst : reset (active high)
-- en : enable (active high), '0' stalls the delay pipeline
-- input : The input to be delayed
-- output : The input after CYCLES cycles elapse (assuming no stalls)
-------------------------------------------------------------------------------

entity delay is
    generic(CYCLES : natural;
            WIDTH  : positive;

            -- Here, we again use an unconstrained vector as a workaround
            -- to VHDL 1993 not allowing the use of generics to define other
            -- generics. See the reg_flexible entity in the reg.vhd example
            -- for an explanation.
            RESET_VALUE   : std_logic_vector := "");
    port(clk    : in  std_logic;

         -- Note that just like we can have default values for generics, we
         -- can also have default values for inputs. Here, I use a default
         -- value of '0' for reset and '1' for enable because to optimize timing
         -- of pipelines, I very often never reset the delay, and always have
         -- it enabled. By providing default values, it saves me from having
         -- to explicitly make these connections in a port map.         
         rst    : in  std_logic := '0';
         en     : in  std_logic := '1';
         input  : in  std_logic_vector(WIDTH-1 downto 0);
         output : out std_logic_vector(WIDTH-1 downto 0));
end delay;


-- Illustrates how to create the delay using flip flop resources. Later examples
-- will illustrate implementations for other resource types.

architecture FF of delay is

    -- FPGA synthesis tools will often try to infer more coarse-grained
    -- primitives for shift regsiters (e.g., embedded memory). In many cases,
    -- this is an effective optimization, but in other cases we might explicitly
    -- want flip-flops. Synthesis tools usually have specific attributes to
    -- turn off such functionality. The example below illustrates how to disable
    -- shift register recognition in Intel Quartus. Xilinx tools likely have
    -- similar attributes.
    
    --attribute altera_attribute       : string;
    --attribute altera_attribute of FF : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";    
    
begin

    -- Handle the special case of delaying by 0 cycles.
    U_CYCLES_EQ_0 : if CYCLES = 0 generate
        output <= input;
    end generate U_CYCLES_EQ_0;

    -- Handle all positive delays.
    U_CYCLES_GT_0 : if CYCLES > 0 generate

        -- Note that unlike the structural delay example, the array here
        -- has a range of 0 to CYCLES-1. The reason for the difference is that
        -- in the structural example, the array represented the chain of
        -- connections between registers. In this example, the array represents
        -- just the outputs of the registers.
        type reg_array_t is array (0 to CYCLES-1) of std_logic_vector(WIDTH-1 downto 0);
        signal regs : reg_array_t;
        
    begin        
        process(clk, rst)
        begin
            if (rst = '1') then
                -- If an initialization value is provided, use it, otherwise
                -- reset to 0.
                if (RESET_VALUE /= "") then
                    for i in 0 to CYCLES-1 loop
                        regs(i) <= RESET_VALUE;
                    end loop;
                else
                    for i in 0 to CYCLES-1 loop
                        regs(i) <= (others => '0');
                    end loop;
                end if;
            elsif (clk'event and clk = '1') then

                if (en = '1') then
                    regs(0) <= input;
                end if;

                -- The if statements isn't needed, but it gets rid of null range
                -- warning when CYCLES = 1
                if (CYCLES > 1) then
                    for i in 0 to CYCLES-2 loop
                        if (en = '1') then
                            regs(i+1) <= regs(i);
                        end if;
                    end loop;
                end if;
            end if;
        end process;

        output <= regs(CYCLES-1);

    end generate U_CYCLES_GT_0;

end FF;
