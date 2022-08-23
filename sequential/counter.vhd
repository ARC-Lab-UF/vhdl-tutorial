-- Greg Stitt
-- University of Florida

-- The examples shows a variety of ways of creating counters, while also
-- illustrating the differences between integer and vector types. The examples
-- also show how to define custom functions and use attributes to simplify
-- the code.

-- TAKEAWAY POINTS:
-- My recommendation is to only use integer types for constants or literals.
-- If you do use them for signals/variables, then the range should be
-- constrained. Even then, constrained integers frequently cause simulation
-- errors. 


-- The following counter entity shows another basic sequential logic example,
-- but compares the usage of the integer type and unsigned type. For this
-- particular example, unsigned is clearly the better choice.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter4bit is
    port (
        clk    : in  std_logic;
        rst    : in  std_logic;
        up     : in  std_logic;
        output : out std_logic_vector(3 downto 0));
end counter4bit;

-- The first architecture illustrates a common problem with the integer type.

architecture BHV_INT_BAD1 of counter4bit is
    -- Use an internal signal to maintain the count. The counter is 4 bits so
    -- we want the range to be from 0 to 15.
    --
    -- IMPORTANT: When using an integer type, you should always constrain the
    -- range of possible values. If you don't, it will synthesize to 32 bits,
    -- which is likely a signficant waste of space. In some cases, synthesis
    -- will be able to optimize away the extra bits, but it is better to
    -- always explicitly constrain it.
    signal count_r : integer range 0 to 15;
    
begin
    -- A counter involves sequential logic, so follow the guidelines for
    -- sequential logic.
    process(clk, rst)
    begin
        if (rst = '1') then
            -- Initialize the count. Integer literals do not use quotes.
            count_r <= 0;
            
        elsif (rising_edge(clk)) then

            -- Implement the basic counter functionality.
            -- Although this looks like it should work, if you simulate this
            -- code, you will receive an error similar to the following:
            --
            -- Fatal: (vsim-3421) Value 16 for count is out of range 0 to 15.
            --
            -- The problem occurs because the integer type will not rollover
            -- from its maximum value (15) to its minimum value (0) when there
            -- is overflow. Instead, when the simulation adds 1 to count when
            -- count = 15, count will become 16. However, we defined count as
            -- only having values from 0 to 15, so 16 is invalid, which causes
            -- the simulation error. The same problem would occur when
            -- subtracting 1 when count = 0.
            --
            -- This problem demonstrates one of the main annoyances of the
            -- integer type. You have to be 100% sure that the signal will
            -- never have a value outside of the specified range.
            --
            -- I almost never use integers for this reason. What makes this
            -- especially problematic is that this code will likely synthesize
            -- with the correct functionality, but you can't simulate it, so
            -- it isn't very useful.
            
            if (up = '1') then
                count_r <= count_r + 1;
            else
                count_r <= count_r - 1;
            end if;
        end if;
    end process;

    -- Send the count to the output. Notice that this is done outside of the
    -- process, otherwise there would be an additional register for output,
    -- which we don't want. To convert from integer to std_logic_vector, you
    -- can first convert to unsigned using the to_unsigned function, assuming
    -- you are using numeric_std.

    output <= std_logic_vector(to_unsigned(count_r, 4));
    
end BHV_INT_BAD1;


-- The following architecture demonstrates another common mistake when using integers.

architecture BHV_INT_BAD2 of counter4bit is

    signal count_r : integer range 0 to 15;
    
begin
    process(clk, rst)
        -- To avoid the problem with the previous architecture, we are going to
        -- add values using a variable, check the variable for overflow, and
        -- then update the count.
        variable temp : integer range 0 to 15;
    begin
        if (rst = '1') then
            count_r <= 0;
        elsif (rising_edge(clk)) then
            
            if (up = '1') then

                -- Get the new count
                temp := count_r + 1;

                -- Check to see if we exceeded the maximum value
                if (temp = 16) then
                    count_r <= 0;
                else
                    count_r <= temp;
                end if;
            else

                -- Get the new count
                temp := count_r - 1;

                -- Check to see if we exceeded the minimum value
                if (temp = -1) then
                    count_r <= 15;
                else
                    count_r <= temp;
                end if;
            end if;

            -- When simulting this code, we get a similar error:
            --
            -- Fatal: (vsim-3421) Value 16 for temp is out of range 0 to 15.
            --
            -- This error occurs for a similar reason as before. When we add
            -- one to count when count = 15, temp needs to be able to store 16.
            -- Likewise, when we subtract one from count when it is 0, temp
            -- needs to store -1. However, we declared temp as having a range
            -- from 0 to 15.
            --
            -- We could potentially fix the problem by changing the range of
            -- temp from -1 to 16, but the following architectures shows a more
            -- concise solution.
            --
            -- The key point to remember is that when using an integer, you
            -- have to be sure that the signal/variable will never exceed that
            -- range. Although it might be obvious for this simple counter, for
            -- complex examples using integers, it can be very difficult.
            
        end if;
    end process;

    output <= std_logic_vector(to_unsigned(count_r, 4));
    
end BHV_INT_BAD2;


-- The following architecture show a correct implementation of the counter when
-- using integers

architecture BHV_INT_GOOD of counter4bit is
    signal count_r : integer range 0 to 15;
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            count_r <= 0;
        elsif (rising_edge(clk)) then
            if (up = '1') then

                -- Instead of using a variable to check for overflow, we can
                -- simply check to see if count is already at the max value. If
                -- so, set it to 0 instead of adding 1.
                
                if (count_r = 15) then
                    count_r <= 0;
                else
                    count_r <= count_r + 1;
                end if;
            else
                -- Similarly, if we are counting down and count = 0, set the
                -- count to 15 insted of subtracting 1.
                
                if (count_r = 0) then
                    count_r <= 15;
                else
                    count_r <= count_r - 1;
                end if;
            end if;
        end if;
    end process;

    output <= std_logic_vector(to_unsigned(count_r, 4));
    
end BHV_INT_GOOD;


-- Although the previous architecture works, we had to use additional code to
-- make sure that count values wrapped around appropriately. If we use unsigned,
-- signed, or std_logic_vector, the wrapping around is handled automatically by
-- the type. 

architecture BHV_UNSIGNED of counter4bit is
    -- Use a 4 bit unsigned instead of an integer.
    signal count_r : unsigned(3 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            count_r <= "0000";
        elsif (rising_edge(clk)) then

            -- Here that we can just add or subtract 1 without any range
            -- check because wrapping will automatically occur.
            if (up = '1') then
                -- Notice that we can actually add an unsigned/signed with an
                -- integer because numeric_std also defines most operators for
                -- integer types. Fortunately, you do not have to worry about
                -- about the 32-bit integer increasing the size of the circuit.
                -- When adding an unsigned/signed with an integer, the integer
                -- is automatically truncated to the width of the
                -- unsigned/signed. 
                count_r <= count_r + 1;
            else
                count_r <= count_r - 1;
            end if;
        end if;
    end process;

    output <= std_logic_vector(count_r);
    
end BHV_UNSIGNED;


---------------------------------------------------------------------


-- The following counter extends the previous counter4bit entity with a generic
-- that specifies the maximum counter value. The minimum value is assumed to be
-- 0. As an exercise, I would recommend adding a MIN_VALUE that supports
-- negative numbers and changing the counter to signed.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity counter_max is
    generic(MAX_VALUE : positive := 15);
    port (
        clk : in std_logic;
        rst : in std_logic;
        up  : in std_logic;

        -- For this entity, the output is not a fixed number of bits. 
        -- The output has MAX_VALUE+1 possible values, so we need to do some
        -- math to calculate the number of bits. If MAX_VALUE+1 was a power
        -- of 2, we could just do log2(MAX_VALUE+1).
        -- 
        -- However, MAX_VALUE is a generic and can be any possible value.
        -- We therefore want to take the ceiling of the log2 to ensure we
        -- have enough bits.
        --
        -- e.g., if the maximum value is 10, the output can have 11 possible
        -- values. log2(11) = 3.4, which will round down to 3 when used as an
        -- integer. However, we know we need 4 bits since 3 bits only supports
        -- 8 values, with a maximum value of 7. By taking the ceiling,
        -- ceil(3.4) = 4, we get the correct number of bits.
        --
        -- To do this calculation in VHDL, we can use the log2 and ceil function
        -- from the math_real package. However, log2 and ceil only support the
        -- real type, so we have to cast the input to real. Similarly, we need
        -- to cast the ceil result back to an integer. The complete, horribly
        -- ugly computation is: integer(ceil(log2(real(MAX_VALUE+1)))). I will
        -- show a more readable way of doing this in the next entity.
        --
        -- Be aware that calculating the appropriate number of bits is a
        -- common place for errors. You should test this for a large number
        -- of generics to be confident that it is correct. A very common
        -- mistake is forgetting the +1. Remember that log2 of a number doesn't
        -- tell you the number of bits needed to represent that number, unless
        -- it is a power of 2. Log2(n) specifies how many bits are required to
        -- represent n different values. Since we also have to support 0, there
        -- are n+1 total values.
        output : out std_logic_vector(integer(ceil(log2(real(MAX_VALUE+1))))-1 downto 0));
end counter_max;


-- In this architecture, we modify the integer implementation from the
-- counter4bit entity by replacing the hardcoded 15 with MAX_VALUE.

architecture BHV_INT of counter_max is

    signal count_r : integer range 0 to MAX_VALUE;

    -- We need the number of bits later, so create a constant.
    -- If you are wondering if we could use a constant when defining
    -- the width of output in the port statement, the answer is sometimes.
    -- You can define constants in a custom package and then use those
    -- constants in port definitions. However, VHDL pre-2008 doesn't allow you
    -- to define constants in packages based on generic values. So, for this
    -- entity, we are stuck with using the bit calculation in the port
    -- definition. We could also create a function that returns the number of
    -- bits, which could be used everywhere, but that will be shown in a later
    -- example.
    constant NUM_BITS : positive := integer(ceil(log2(real(MAX_VALUE+1))));
    
begin
    
    process(clk, rst)
    begin
        if (rst = '1') then
            count_r <= 0;
        elsif (rising_edge(clk)) then
            if (up = '1') then
                if (count_r = MAX_VALUE) then
                    count_r <= 0;
                else
                    count_r <= count_r + 1;
                end if;
            else
                if (count_r = 0) then
                    count_r <= MAX_VALUE;
                else
                    count_r <= count_r - 1;
                end if;
            end if;
        end if;
    end process;

    -- We can't simply hardcode a width of 4 anymore, so we use the constant.
    output <= std_logic_vector(to_unsigned(count_r, NUM_BITS));
    
end BHV_INT;


-- This architecture modifies the unsigned implementation of counter4bit to
-- support a generic MAX_VALUE. Note that the previous advantages of unsigned
-- do not apply here because MAX_VALUE does not have to be a power of 2 - 1.
-- In fact, in this case the integer and unsigned implementations are very
-- similar.

architecture BHV_UNSIGNED of counter_max is

    -- We have to recalculate the bits here again. It would definitely be
    -- best to use a function in a package. See the next entity.
    constant NUM_BITS : positive := integer(ceil(log2(real(MAX_VALUE+1))));

    -- when using the generic MAX_VALUE, count requires NUM_BITS bits.
    signal count_r : unsigned(NUM_BITS-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            -- We can't use a hardcoded literal anymore because the width can
            -- change with MAX_VALUE. We also can't assign an integer to
            -- a vector (without using conversion functions).
            count_r <= (others => '0');
            
        elsif (rising_edge(clk)) then

            -- We can no longer simply add/subtract 1 because MAX_VALUE may not
            -- be a power of 2 - 1 (e.g., 3,7,15,etc.). Instead, we have to use
            -- a similar strategy as the integer implementation.

            if (up = '1') then
                if (count_r = MAX_VALUE) then
                    count_r <= to_unsigned(0, NUM_BITS);
                else
                    count_r <= count_r + 1;
                end if;
            else
                if (count_r = 0) then
                    count_r <= to_unsigned(MAX_VALUE, NUM_BITS);
                else
                    count_r <= count_r - 1;
                end if;
            end if;
        end if;
    end process;

    output <= std_logic_vector(count_r);
    
end BHV_UNSIGNED;


-------------------------------------------------------------------------

-- Greg Stitt
-- University of Florida

-- This examples shows how to extend the previous counter entity with a custom
-- package that includes a function for computing the number of bits, which
-- makes the code much more concise. Make sure to also look at counter_pkg.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- Include a custom package that defines a function for computing the number of
-- bits in signal.
use work.counter_pkg.all;

entity counter_max2 is
    generic(MAX_VALUE : positive := 15);
    port (
        clk : in std_logic;
        rst : in std_logic;
        up  : in std_logic;


        -- In this version, we replace the ugly computation:
        --
        -- integer(ceil(log2(real(MAX_VALUE+1))))
        --
        -- with a clog2 function that we added to counter_pkg.

        output : out std_logic_vector(clog2(MAX_VALUE+1)-1 downto 0));
end counter_max2;


architecture BHV_INT of counter_max2 is

    signal count_r : integer range 0 to MAX_VALUE;
    
begin
    
    process(clk, rst)
    begin
        if (rst = '1') then
            count_r <= 0;
        elsif (rising_edge(clk)) then
            if (up = '1') then
                if (count_r = MAX_VALUE) then
                    count_r <= 0;
                else
                    count_r <= count_r + 1;
                end if;
            else
                
                if (count_r = 0) then
                    count_r <= MAX_VALUE;
                else
                    count_r <= count_r - 1;
                end if;
            end if;
        end if;
    end process;

    -- Since the previously used constant just represented the number of bits
    -- in the output, we can access the same information via the 'length
    -- attribute.
    output <= std_logic_vector(to_unsigned(count_r, output'length));
    
end BHV_INT;


architecture BHV_UNSIGNED of counter_max2 is

    -- We previously used the NUM_BITS constant to define the range of the
    -- count, but we can just use the output's range attribute and get rid
    -- of the constant.
    signal count_r : unsigned(output'range);
    
begin
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

    output <= std_logic_vector(count_r);
    
end BHV_UNSIGNED;



--------------------------------------------------------------------------
-- Top-level entity for evaluating each of the different implementations.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.counter_pkg.all;

entity counter is
    generic(MAX_VALUE : positive := 15);
    port (
        clk    : in  std_logic;
        rst    : in  std_logic;
        up     : in  std_logic;
        output : out std_logic_vector(clog2(MAX_VALUE+1)-1 downto 0));
end counter;

architecture default_arch of counter is
begin

    -- INSTRUCTIONS: uncomment the implementation you want to evaluate. If
    -- testing counter4bit, make sure to commen out the generic map. Also make
    -- sure to set MAX_VALUE to 15 in the testbench.
    
    --U_COUNTER : entity work.counter4bit(BHV_INT_BAD1)
    --U_COUNTER : entity work.counter4bit(BHV_INT_BAD2)
     --U_COUNTER : entity work.counter4bit(BHV_INT_GOOD)
    --U_COUNTER : entity work.counter4bit(BHV_UNSIGNED)
        --U_COUNTER : entity work.counter_max(BHV_INT)
    --U_COUNTER : entity work.counter_max(BHV_UNSIGNED)
    --U_COUNTER : entity work.counter_max2(BHV_INT)
    U_COUNTER : entity work.counter_max2(BHV_UNSIGNED)
        generic map (MAX_VALUE => MAX_VALUE)
        port map (
            clk    => clk,
            rst    => rst,
            up     => up,
            output => output);

end default_arch;

