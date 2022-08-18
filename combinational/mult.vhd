-- Greg Stitt
-- University of Florida

-- This exact demonstrates a number of different ways of implementing
-- a multiplier. There is a mult entity at the bottom of the file that acts
-- as a top level to evaluate the different implementations.

-- Entity: mult1
-- Description: Implements a multiplier that prevents overflow by producing
-- a product that is twice the width of the inputs. It uses a generic to
-- specify the input width, and whether or not the inputs are signed.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mult1 is
    generic (
        INPUT_WIDTH : positive;
        IS_SIGNED   : boolean
        );
    port (
        in0, in1 : in  std_logic_vector(INPUT_WIDTH-1 downto 0);
        product  : out std_logic_vector(INPUT_WIDTH*2-1 downto 0)
        );
end mult1;


architecture arch1 of mult1 is
begin
    -- A multiplier is combinational logic, so we still make sure to include
    -- all inputs in the sensitivity list.
    process(in0, in1)
    begin
        -- Check the signedness to determine how to case the inputs. Like
        -- addition, numeric_std requires multiplication to have either signed
        -- or unsigned inputs, which makes sense in this case since the product
        -- depends on the sign.
        if (IS_SIGNED) then
            -- We don't have to do anything extra to get a product that is
            -- twice the width of the inputs because the multiplication operator
            -- returns a signal whose width is the sum of the widths of the
            -- inputs.
            --
            -- If you want a truncated output, things get a little tricker.
            -- I haven't checked the VHDL standard for a specific definition,
            -- but some tools will let you assign a multiplication to a smaller
            -- signal, basically handling the truncation automatically. However,
            -- others will not. In my test, Modelsim automatically truncated
            -- the multiplication, where as Quartus report an error about a
            -- width mismatch.
            --
            -- To work across all tools, it is best to always use a product
            -- whose width is the sum of input widths. If you don't need
            -- all the bits, simply ignore them. This unfortunately can cause
            -- warnings in some FPGA tools, which can be annoying, but is still
            -- the most portable approach.
            product <= std_logic_vector(signed(in0) * signed(in1));
        else
            product <= std_logic_vector(unsigned(in0) * unsigned(in1));
        end if;
    end process;
end arch1;


architecture arch2 of mult1 is
begin
    -- We can use concurrent assignments, but need extra logic to check the
    -- signedness. We could potentially use when-else or with-select, but we
    -- can also an if-generate. These will be explored more in the structural
    -- architecture section.
    U_SIGNED : if (IS_SIGNED) generate
        product <= std_logic_vector(signed(in0) * signed(in1));
    end generate;

    -- It's annoying, but VHDL 1993 does not have an else generate construct,
    -- so we need to explicitly state the else condition.
    U_UNSIGNED : if (not IS_SIGNED) generate
        product <= std_logic_vector(unsigned(in0) * unsigned(in1));
    end generate;

end arch2;


-- VHDL 2008 introduced an else generate, which simplifies the previous
-- architecture.

--architecture arch_2008 of mult1 is
--begin
--    U_MULT : if (IS_SIGNED) generate
--        product <= std_logic_vector(signed(in0) * signed(in1));
--    else generate
--        product <= std_logic_vector(unsigned(in0) * unsigned(in1));
--    end generate;
--end arch_2008;


-- Entity: mult_high_low
-- Description: This entity implements the same functionality, but instead
-- of single product, it divides the product into a high and low signal.
-- Technically, this is never needed because you could always split the output
-- of the previous entity, but it demonstrates some new constructs.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mult_high_low is
    generic (
        INPUT_WIDTH : positive;
        IS_SIGNED   : boolean
        );
    port (
        in0, in1  : in  std_logic_vector(INPUT_WIDTH-1 downto 0);
        high, low : out std_logic_vector(INPUT_WIDTH-1 downto 0)
        );
end mult_high_low;

architecture arch1 of mult_high_low is
begin
    process(in0, in1)
        variable temp : std_logic_vector(INPUT_WIDTH*2-1 downto 0);
    begin
        -- Here we use a variable to get the full product, and then slice
        -- into that product to create high and low. It is important that
        -- we use a variable for temp, or this won't work correctly.
        if (IS_SIGNED) then
            temp := std_logic_vector(signed(in0) * signed(in1));
        else
            temp := std_logic_vector(unsigned(in0) * unsigned(in1));
        end if;

        high <= temp(INPUT_WIDTH*2-1 downto INPUT_WIDTH);
        low  <= temp(INPUT_WIDTH-1 downto 0);
    end process;
end arch1;


-- In this example, we avoid the need for a variable by assigning the temp
-- signal in the process, and assigning high and low concurrently. Splitting
-- functionality across sequential and concurrent statements can often be a
-- convenient strategy.

architecture arch2 of mult_high_low is
    signal temp : std_logic_vector(INPUT_WIDTH*2-1 downto 0);
begin
    process(in0, in1)
    begin
        if (IS_SIGNED) then
            temp <= std_logic_vector(signed(in0) * signed(in1));
        else
            temp <= std_logic_vector(unsigned(in0) * unsigned(in1));
        end if;
    end process;

    -- Concurrent assignments.
    high <= temp(INPUT_WIDTH*2-1 downto INPUT_WIDTH);
    low  <= temp(INPUT_WIDTH-1 downto 0);
end arch2;

-- In VHDL 2008, we can assign vector as part of LHS aggregates, which avoids
-- the need for an extra signal.

--architecture arch_2008 of mult_high_low is
--begin
--    process(in0, in1)
--    begin
--        if (IS_SIGNED) then
--            (high, low) <= std_logic_vector(signed(in0) * signed(in1));
--        else
--            (high, low) <= std_logic_vector(unsigned(in0) * unsigned(in1));
--        end if;
--    end process;
--end arch_2008;


-- Entity: mult
-- Description: Top-level entity for evaluating all the previous
-- implementations.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mult is
    generic (
        INPUT_WIDTH : positive := 16;
        IS_SIGNED   : boolean  := false
        );
    port (
        in0, in1 : in  std_logic_vector(INPUT_WIDTH-1 downto 0);
        product  : out std_logic_vector(INPUT_WIDTH*2-1 downto 0)
        );
end mult;

architecture default_arch of mult is
begin
    -- INSTRUCTION: uncommend the architecture you would like to evalute.
    -- If testing the mult_high_low implementaitons, you must commend out the
    -- entire generic map and port map for mult1.
    
    U_MULT : entity work.mult1(arch1)
     --U_MULT : entity work.mult1(arch2)
     --U_MULT : entity work.mult1(arch_2008)
        generic map (
            INPUT_WIDTH => INPUT_WIDTH,
            IS_SIGNED   => IS_SIGNED)
        port map (
            in0     => in0,
            in1     => in1,
            product => product
            );


    --U_MULT : entity work.mult_high_low(arch1)
    --U_MULT : entity work.mult_high_low(arch2)
    --U_MULT : entity work.mult_high_low(arch_2008)
        --generic map (
        --    INPUT_WIDTH => INPUT_WIDTH,
        --    IS_SIGNED => IS_SIGNED)
        --port map (
        --    in0 => in0,
        --    in1 => in1,
        --    high => product(INPUT_WIDTH*2-1 downto INPUT_WIDTH),
        --    low => product(INPUT_WIDTH-1 downto 0)
        --    );


end default_arch;
