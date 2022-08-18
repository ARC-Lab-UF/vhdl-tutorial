-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;

-- Needed for to_unsigned function.
use ieee.numeric_std.all;

-- Needed for ceil and log2 functions, and the real type.
use ieee.math_real.all;

entity priority_encoder is
    generic (
        -- Generics are compile-time-specified parameters that allow you to
        -- create flexible entities for different use cases. In this example,
        -- we create a generic (i.e. parameter) for the number of inputs. The
        -- assigned value is optional and provides a default value in the case
        -- that a particular instance does not specify a value (see structural
        -- architecture section).
        --
        -- The positive type is a subset of the integer type that only allows
        -- positive integers. Since a priority encoder has to have at least 1
        -- input, it makes sense to make this positive instead of integer.
        --
        -- VERILOG LIMITATION: Verilog/SV does not have built in ways of doing
        -- parameter validation. You can find informal workarounds, but VHDL
        -- has much more formal support for validation parameter values at
        -- compile time.

        NUM_INPUTS : positive := 4
        );    
    port (
        inputs : in  std_logic_vector(NUM_INPUTS-1 downto 0);
        valid  : out std_logic;
        -- To determine the number of bits in the output, we need to take the
        -- ceiling of log of the number of inputs. Here is one example of where
        -- VHDL syntax can get annoying. VHDL is strongly typed and requires
        -- explicit conversions between types. The log2 and ceil functions
        -- are only defined for the real type, which is similar to a float in
        -- other languages. We then need to cast the result back to integer
        -- because we can't use a real type in a range.
        --
        -- Ideally, you would define a NUM_OUTPUTS constant based on the
        -- NUM_INPUTS generic. There is no simple way of doing this in pre-2008
        -- VHDL, but the tutorial will illustrate several workarounds in later
        -- examples.
        result : out std_logic_vector(integer(ceil(log2(real(NUM_INPUTS))))-1 downto 0)
        );
end priority_encoder;


architecture ARCH1 of priority_encoder is

    constant NUM_OUTPUTS : integer := integer(ceil(log2(real(NUM_INPUTS))));
begin
    
    process(inputs)
    begin
        valid  <= '0';
        result <= (others => '0');

        -- Since we don't know the number of inputs, we have to use a loop
        -- to define the behavior. VHDL has a for loop construct, which is
        -- a sequential statement, and therefore has to be inside a process.

        -- In this architecture, we iterate up from 0, which naturally handles
        -- the desired priority since later iterations will change the value
        -- of the result when appropriate.
        
        for i in 0 to NUM_INPUTS-1 loop
            if (inputs(i) = '1') then
                result <= std_logic_vector(to_unsigned(i, NUM_OUTPUTS));
                valid  <= '1';
            end if;
        end loop;
    end process;
end ARCH1;

architecture ARCH2 of priority_encoder is

    constant NUM_OUTPUTS : integer := integer(ceil(log2(real(NUM_INPUTS))));
begin
    
    process(inputs)
    begin
        valid  <= '0';
        result <= (others => '0');

        -- In this architecture, we iterate down from NUM_INPUTS-1. The
        -- advantage of this approach is that we can use the break statement
        -- to exit the loop after finding the first asserted input bit. While
        -- this shouldn't have an effect on synthesis (both architectures
        -- synthesize the same in my tests), this architecture might have
        -- faster simulation times.
        
        for i in NUM_INPUTS-1 downto 0 loop
            if (inputs(i) = '1') then
                result <= std_logic_vector(to_unsigned(i, NUM_OUTPUTS));
                valid  <= '1';
                exit;
            end if;
        end loop;
    end process;
end ARCH2;


architecture default_arch of priority_encoder is
begin
    -- INSTRUCTIONS: Change the architecture name to simulate/synthesize
    -- each architecture
    UUT : entity work.priority_encoder(ARCH1)
        generic map (NUM_INPUTS => NUM_INPUTS)
        port map (inputs => inputs,
                  valid  => valid,
                  result => result);
end default_arch;



