-- Greg Stitt
-- University of Florida

-- Entity: Add
-- Description: An adder with a generic for the width of the inputs and sum,
--              and a separate carry out bit. This example does not have a
--              carry in, but can be easily extended.
--
-- This is a lengthy example that demonstrates how to perform the addition using
-- three different package combinations. Each combination has its own separate
-- entity. There is then a top-level entity called add at the bottom of the
-- file. Change the instantiated architecture and entity in the add entity
-- to synthesize/simulate each implementation.
--
-- NOTE: If you don't want to read over the entire file, just look at the
-- alu_ns entity, which follows my recommendations.


library ieee;
use ieee.std_logic_1164.all;

-- To perform arithmetic operations in VHDL, we have a choice of several
-- packages. I highly recommend numeric_std. Compared to other alternatives, it
-- may seem verbose because it requires most arithmetic and comparison operators
-- to use the unsigned or signed types, but its consistency is a huge
-- advantage. The alternatives will be shown in other examples.
use ieee.numeric_std.all;


-- Note:
-- You may wonder why the I/O isn't of type unsigned/signed. This is possible,
-- but is not suggested because for whatever reason, many tools expect
-- std_logic_vector for all top-level I/O. If you make the I/O unsigned,
-- then you might not be able to use it as a top-level entity.

entity add_ns is
    generic (
        WIDTH : positive
        );    
    port (
        in0, in1  : in  std_logic_vector(WIDTH-1 downto 0);
        sum       : out std_logic_vector(WIDTH-1 downto 0);
        carry_out : out std_logic
        );
end add_ns;


architecture bad of add_ns is

    -- One trick to get the carry out is to convert the addition from WIDTH bits
    -- to WIDTH+1 bits. We use this temp signal here to store the WIDTH+1 bit
    -- sum. We also make it unsigned to reduce some casting.
    signal temp : unsigned(WIDTH downto 0);
begin

    -- An adder is combinational logic, so remember to use combination
    -- synthesis guideline 1 (put all inputs in sensitivity list). Or, use
    -- "all" when using VHDL 2008.    
    process(in0, in1)
    begin

        -- in0 and in1 must be cast to unsigned (or signed) because + (or any
        -- arithmetic or comparison operator) is not defined for
        -- std_logic_vector in the numeric_std package.
        --
        -- resize (i.e. sign extension) is used to convert both inputs to
        -- WIDTH+1 bits, whose addition provides a do WIDTH+1 bit result. 

        -- COMMON PROBLEM: This may look right, but signals are not updated
        -- until the end of the process (or on a wait statement), which means
        -- that sum and carry are assigned values based on the previous value
        -- of temp, not the value that was just calculated. In addition, the
        -- first time the process simulates, sum and carry will be undefined
        -- because temp has not been assigned a value yet.
        --
        -- KEY POINT TO REMEMBER: Signals are updated at the end of a process,
        -- or on a wait statement. I don't use wait statements in synthesizable
        -- code, so you only have to conisder that issue in a testbench.

        temp <= resize(unsigned(in0), WIDTH+1) + resize(unsigned(in1), WIDTH+1);

        -- Slice into the temp signal to get the sum bits and the carry out bit.
        -- We have to explicitly cast temp back to std_logic_vector due to
        -- strong typing. This is one of the annoyances that make the other
        -- arithmetic packages look attractive. Despite the annoyances, I still
        -- strongly recommend numeric_std.
        sum       <= std_logic_vector(temp(WIDTH-1 downto 0));
        carry_out <= temp(WIDTH);
    end process;
end bad;


architecture good1 of add_ns is
begin
    process(in0, in1)

        -- SOLUTION: Make temp a variable instead of a signal. Variables are
        -- updated immediately. Note that the assignment operation for a
        -- variable is ":=" and not "<=".
        --
        -- SYSTEM VERILOG COMPARISON: Verilog does not have separate concepts
        -- for signals and variables. Instead, it has a single "signal"
        -- construct and two different assignment operators for blocking
        -- assignments (equivalent to VHDL variables) and non-blocking
        -- assignments (equivalent to VHDL signals).
        --
        -- Note that VHDL variables have a scope that is limited to a process.
        -- There is a very good reason for this restriction. Verilog allows
        -- blocking assignments of "signals" in multiple places, which leads
        -- to race conditions that provide non-deterministic behavior. The
        -- flexibility of Verilog has advantages, but requires a strong
        -- understanding of how to avoid race conditions. VHDL variables
        -- eliminate this risk, making it easier to learn and safer to use.
        
        variable temp : unsigned(WIDTH downto 0);

    begin

        temp      := resize(unsigned(in0), WIDTH+1) + resize(unsigned(in1), WIDTH+1);
        sum       <= std_logic_vector(temp(WIDTH-1 downto 0));
        carry_out <= temp(WIDTH);
    end process;
end good1;


architecture good2 of add_ns is
begin
    process(in0, in1)
        variable temp : unsigned(WIDTH downto 0);
    begin
        -- The following is a slightly simplified version of the previous
        -- architecture that only resizes one input. This code works because
        -- the numeric_std addition operator returns a signal whose
        -- width is the maximum width of the inputs. So, as long as one input
        -- is WIDTH+1 bits, we get the right width for the result.
        
        temp      := resize(unsigned(in0), WIDTH+1) + unsigned(in1);
        sum       <= std_logic_vector(temp(WIDTH-1 downto 0));
        carry_out <= temp(WIDTH);
    end process;
end good2;


architecture good3 of add_ns is
begin
    process(in0, in1)
        -- Here we make temp std_logic_vector to illustrate different
        -- casting alternatives. In general, I tend to use the type that
        -- minimizes casting, unless the signedness if important, in which
        -- case I will always use unsigned/signed.
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- Because we made temp std_logic_vector, we now have to cast the
        -- result of the addition back to std_logic_vector.        
        temp      := std_logic_vector(resize(unsigned(in0), WIDTH+1) + unsigned(in1));
        -- Similarly, we don't need any casting on this line now.
        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);
    end process;
end good3;


architecture good4 of add_ns is
begin
    process(in0, in1)
        variable temp : unsigned(8 downto 0);
    begin
        -- Instead of using resize, you can also concatenate a "0" bit onto
        -- each (or just one) input, which effectively creates a WIDTH+1-bit
        -- addition. Although more concise, the disadvantage is that if you
        -- care about sign extension (we don't here), using resize is more
        -- appropriate.
        
        temp      := unsigned("0"&in0) + unsigned(in1);
        sum       <= std_logic_vector(temp(7 downto 0));
        carry_out <= temp(WIDTH);
    end process;
end good4;


architecture good5 of add_ns is
    signal temp : unsigned(WIDTH downto 0);
begin
    -- This is the code from the bad architecture where we assign a temp signal
    -- instead of a variable. You might be surprised to find out this works
    -- correctly in this situation. The key difference is that the code in this
    -- architecture is outside of a process, so it uses concurrent statements.
    -- With a concurrent statement, the LHS updates anytime the RHS changes, so
    -- it doesn't have the same problem as the bad architecture.
    --
    -- VERILOG COMPARISON: Whereas VHDL uses the same syntax for concurrent and
    -- sequential signal assignments, Verilog uses an explicit "assign" contruct
    -- for concurrent assignments. Verilog also puts restrictions on what can
    -- be assigned concurrently, but this has been relaxed by the new "logic"
    -- type in SystemVerilog.
    
    temp      <= resize(unsigned(in0), WIDTH+1) + resize(unsigned(in1), WIDTH+1);
    sum       <= std_logic_vector(temp(WIDTH-1 downto 0));
    carry_out <= temp(WIDTH);
end good5;


-- VHDL 2008 allows aggregates on the LHS of assignments, so we can do the
-- entire behavior with one line. Technically, VHDL 1993 also allows LHS
-- aggregates, but you can't include a vector in the aggregate, so we can't
-- do this without 2008.
--
-- This is commented out because of the lack of tool support. In my tests, it
-- worked in Modelsim (after enabling 2008), but did not work in
-- Quartus Prime 20.1, even after enabling 2008 support. This is a perfect
-- example of inconsistent tool support making many of the VHDL 2008 much less
-- useful.

--architecture good_2008 of add_ns is
--begin
--    (carry_out, sum) <= std_logic_vector(resize(unsigned(in0), WIDTH+1) + resize(unsigned(in1), WIDTH+1));
--end good_2008;


-------------------------------------------------------------------------
-- Adder using std_logic_arith with std_logic_unsigned.
-------------------------------------------------------------------------

-- Every entity must be preceded by the library and package declarations, even
-- if they already have been specified for another entity in the same file.
-- In general, I usually only include one entity per file to make it easier
-- to know where an entity is defined. In this case, I only use multiple
-- entities in this file to simplify the structure of the tutorial.

library ieee;
use ieee.std_logic_1164.all;

-- Here we use both std_logic_arith (an alternative to numeric_std) combined
-- with std_logic_unsigned, which treats std_logic_vectors as unsigned to
-- reduce casting. Similarly, there is a std_logic_signed for signed casting.
-- Although you will see code examples using these packages,
-- I strongly discourage their use for two reasons. Most importantly, there
-- are actually different variations from different tool vendors, which makes
-- your code specific to one tool. Second, there are strange exceptions you
-- have to remember that result in hard to understand error mesesages.
--
-- If you really can't live with the casting required by numeric_std, then
-- VHDL 2008 adds a numeric_std_unsigned package that serves the same role as
-- the std_logic_unsigned package. Ultimately, I still rarely recommend that
-- unless you know for sure your entity will never need different signedness.
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity add_sla_us is
    generic (
        WIDTH : positive);
    port (
        in0       : in  std_logic_vector(WIDTH-1 downto 0);
        in1       : in  std_logic_vector(WIDTH-1 downto 0);
        sum       : out std_logic_vector(WIDTH-1 downto 0);
        carry_out : out std_logic
        );
end add_sla_us;


architecture BHV1 of add_sla_us is
begin  -- BVH1
    
    process (in0, in1)
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- With std_logic_arith and std_logic_unsigned, we can add std_logic
        -- vectors (which are then treated as unsigned),
        -- so we just need to convert the inputs to a width of WIDTH+1.
        -- conv_std_logic_vector can handle size conversions, but we must first
        -- cast the inputs to unsigned because the first parameter has to be
        -- unsigned, signed, or an integer. 

        temp      := conv_std_logic_vector(unsigned(in0), WIDTH+1) + conv_std_logic_vector(unsigned(in1), WIDTH+1);
        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);
    end process;
end BHV1;


architecture BHV2 of add_sla_us is
begin  -- BHV2
    
    process (in0, in1)
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- An alternative way to do the add is to convert the inputs to WIDTH+1
        -- wide unsigned values using conv_unsigned and add the unsigned values.
        -- It might seem confusing that the code casts the inputs to unsigned
        -- before calling conv_unsigned, but this is necessary because
        -- conv_unsigned does not take std_logic_vector inputs.

        -- *****Important*******
        -- You do not need to cast the result to std_logic_vector. In fact, if
        -- you do, you will get a compilation/synthesis error. The reason is
        -- complicated, but here's a summary. std_logic_arith overloads the +
        -- function with different possible return types. When trying to cast
        -- the result, the compiler/sythesis tool doesn't know which version of
        -- the function to use. Basic rule: when using std_logic_arith, don't
        -- cast the output of arithmetic operations.

        temp      := conv_unsigned(unsigned(in0), WIDTH+1) + conv_unsigned(unsigned(in1), WIDTH+1);
        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);
    end process;
end BHV2;


architecture BHV3 of add_sla_us is
begin  -- BHV3

    process (in0, in1)
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- Here we can add the two vectors without any casting. This is the
        -- primary reason why people use both std_logic_arith and
        -- std_logic_unsigned.
        temp      := ("0"&in0) + ("0"&in1);
        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);

    end process;

end BHV3;

-------------------------------------------------------------------------
-- Adder using std_logic_arith without any other packages.
-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-- Here we use std_logic_arith by itself without std_logic_unsigned.
-- I don't recommend doing this.
use ieee.std_logic_arith.all;

entity add_sla is
    generic (
        WIDTH : positive);
    port (
        in0       : in  std_logic_vector(WIDTH-1 downto 0);
        in1       : in  std_logic_vector(WIDTH-1 downto 0);
        sum       : out std_logic_vector(WIDTH-1 downto 0);
        carry_out : out std_logic
        );
end add_sla;


architecture BHV1 of add_sla is
begin
    process (in0, in1)
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- With std_logic_arith and *without* std_logic_unsigned, we have to
        -- explicitly convert the inputs to unsigned before adding. This code
        -- uses conv_unsigned (the std_logic_arithm equivalent to resize) to
        -- sign extend the inputs to create the carry.
        --
        -- It might seem confusing that the code casts the inputs to unsigned
        -- before calling conv_unsigned, but this is necessary because
        -- conv_unsigned does not take std_logic_vector inputs.

        -- *****Important*******
        -- You don't need to cast the add result to std_logic_vector. In fact,
        -- if you do, you will get a compilation/synthesis error. The reason is
        -- complicated, but here's a summary. std_logic_arith overloads the +
        -- function with different possible return types. When trying to cast
        -- the result, the compiler/sythesis tool doesn't know which version of
        -- the function to use. Basic rule: when using std_logic_arith, don't
        -- cast the output of arithmetic operations.

        temp := conv_unsigned(unsigned(in0), WIDTH+1) + conv_unsigned(unsigned(in1), WIDTH+1);

        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);
    end process;
end BHV1;


architecture BHV2 of add_sla is

begin  -- BHV2

    process (in0, in1)
        variable temp : std_logic_vector(WIDTH downto 0);
    begin
        -- Same code as std_logic_arith with std_logic_unsigned
        -- Make sure not to cast result to std_logic_vector (see previous
        -- comments)        
        temp      := unsigned("0"&in0) + unsigned("0"&in1);
        sum       <= temp(WIDTH-1 downto 0);
        carry_out <= temp(WIDTH);
    end process;
end BHV2;


-------------------------------------------------------------------------
-- Top-level adder used to evaluate the other adder entities and architectures.
-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity add is
    generic (
        WIDTH : positive := 8
        );    
    port (
        in0, in1  : in  std_logic_vector(WIDTH-1 downto 0);
        sum       : out std_logic_vector(WIDTH-1 downto 0);
        carry_out : out std_logic
        );
end add;

architecture default_arch of add is
begin

    -- INSTRUCTIONS: Change the architecture and/or entity that is instantiated
    -- to evaluate each adder implementation.
    
    --U_ADD : entity work.add_ns(bad)
    U_ADD : entity work.add_ns(good1)
        --U_ADD : entity work.add_ns(good2)
        --U_ADD : entity work.add_ns(good3)
        --U_ADD : entity work.add_ns(good4)
        --U_ADD : entity work.add_ns(good5)
        --U_ADD : entity work.add_ns(good_2008)
        --U_ADD : entity work.add_sla_us(BHV1)
        --U_ADD : entity work.add_sla_us(BHV2)
        --U_ADD : entity work.add_sla_us(BHV3)
        --U_ADD : entity work.add_sla(BHV1)
        --U_ADD : entity work.add_sla(BHV2)
        generic map (WIDTH => WIDTH)
        port map (in0       => in0,
                  in1       => in1,
                  sum       => sum,
                  carry_out => carry_out);

end default_arch;
