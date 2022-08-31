-- Greg Stitt
-- University of Florida

-- This example demonstrates how synthesis tools convert sequential logic
-- descriptions into a circuit. There is one rule that is critically important
-- to remember:
--
-- If you assign a signal (which also includes outputs) on a
-- rising clock edge, the synthesis tool will infer a register.
--
-- It is common to accidentally introduce registers by assigning signals on a
-- rising clock. Always keep in mind exactly how many registers you want. If
-- you have to assign a signal and you don't want it to become a register, you
-- have to assign it outside of the process used for sequential logic. Do not
-- simply move it outside the rising clock elsif in the same process. This
-- violates my synthesis guidelines for sequential logic. If you don't want a
-- register on a particular assignment, that means that the assignment
-- corresponds to an output of combinational logic. You should therefore move
-- the assignment into concurrent statements, or a separate process that
-- follows my guidelines for combinational logic.

-- INSTRUCTIONS: Please see architectures.pdf for the corresponding schematic
-- of each of the following architectures. Also, use top_level.vhd to
-- synthesize each architecture separately, so you can confirm the synthesize
-- circuit matches the circuit in the pdf.


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity example is
    generic(
        WIDTH : positive);
    port (
        clk  : in  std_logic;
        rst  : in  std_logic;
        in1  : in  std_logic_vector(WIDTH-1 downto 0);
        in2  : in  std_logic_vector(WIDTH-1 downto 0);
        in3  : in  std_logic_vector(WIDTH-1 downto 0);
        out1 : out std_logic_vector(WIDTH-1 downto 0);
        out2 : out std_logic_vector(WIDTH-1 downto 0));
end example;


architecture ARCH1 of example is
begin
    -- Note that despite having combinational logic in the architecture, we are
    -- still using the sequential logic guidelines, which can apply to any
    -- circuit that contains sequential logic. There are a few exceptions,
    -- which I will demonstrate in ARCH3.
    
    process(clk, rst)
    begin
        if (rst = '1') then
            -- For this example, assume that all registers should be reset to 0.
            out1 <= (others => '0');
            out2 <= (others => '0');
            
        elsif(rising_edge(clk)) then

            -- Assigning a signal on a rising clock edge creates a register,
            -- so this one line of code will instantiate the adder and the
            -- register for out1
            out1 <= std_logic_vector(unsigned(in1) + unsigned(in2));

            -- Assigning out2 on a rising clock creates the second register and
            -- connects it to in3.
            out2 <= in3;
        end if;
    end process;
end ARCH1;


architecture ARCH2 of example is

    -- ARCH2 has two registers not connected to outputs, which each require an
    -- internal signal.
    --
    -- NAMING CONVENTION: For any internal signal that is a register, I use
    -- a _r suffix. I make an exception for outputs because I generally don't
    -- expose implementation details on the port, and there are situations
    -- where an output may or may not be registered based on different
    -- conditions. In situations, to ensure that every register has a _r suffix,
    -- I will create an additional internal signal with the output name and _r
    -- suffix. I will then simply do a concurrent assignment of the _r version
    -- to the actual output.
    --
    -- Another naming convention that I like is to use a _q suffix on a register
    -- output, and a _d suffix for register inputs. There is a lot I like about
    -- this convention, but I haven't adopted it for several reasons. First,
    -- my register inputs often come directly from other signals, so I don't
    -- want to introduce an extra _d signal. However, I've found it can be very
    -- useful for 2-process FSMD examples, which are explained later.
    
    signal in1_r, in2_r : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            out1 <= (others => '0');
            out2 <= (others => '0');

            -- Rest the new registers the same way as before.
            in1_r <= (others => '0');
            in2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then

            -- For ARCH2, in1 and in2 each pass through a register before
            -- being added. We can add these registers by simply assigning the
            -- corresponding signals on the rising clock edge, which will
            -- synthesize two registers.
            
            in1_r <= in1;
            in2_r <= in2;

            -- The remainder of the circuit stays the same, with the exception
            -- of the adder inputs now coming from the two new registers.

            out1 <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
            out2 <= in3;

        -- IMPORTANT:
        -- Note that the order of these statements (despite being
        -- sequential statements) does not affect the functionality or the
        -- synthesized circuit. It is important to remember that signals
        -- are only updated at the end of the process (technically at the next
        -- step), so the addition operation will always use the previous value
        -- of in1_r and in2_r regardless of the ordering of statements.
        -- In fact, this behavior matches the one-cycle delay introduced by
        -- the two input registers, which is exactly what we want.
        end if;
    end process;
end ARCH2;


architecture ARCH2_2 of example is

    -- In this version of ARCH2, we create explicit signals for the two output
    -- registers to ensure all registers are explicit.
    signal in1_r, in2_r, out1_r, out2_r : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            
        elsif(rising_edge(clk)) then

            in1_r <= in1;
            in2_r <= in2;

            -- Here we assign the _r version of the outputs.
            out1_r <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
            out2_r <= in3;
        end if;
    end process;

    -- This concurrent assignemnt connects the explicit registers to the
    -- outputs. This code synthesized to the exact same circuit. I used to
    -- avoid this strategy, but have come to appreciate it more for complex
    -- entities, especially when doing timing optimization.

    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH2_2;


architecture ARCH3 of example is

    signal in1_r, in2_r : std_logic_vector(WIDTH-1 downto 0);

    -- Input 3 now needs a register signal (in3_r) because unlike the previous
    -- architecture, that register no longer connects to an output.
    signal in3_r : std_logic_vector(WIDTH-1 downto 0);

    -- The register on the output of the adder (add_out1_r) now requires an
    -- internal signal. Despite still connecting directly to output out1, the
    -- register also connects to another register (add_out2_r). To achieve this,
    -- we either need to be able to read from out1 (not possible in VHDL 1993),
    -- or we need a separate signal for the register.
    signal add_out1_r : std_logic_vector(WIDTH-1 downto 0);

    -- Register add_out2_r does not solely connect to an output, and therefore
    -- needs an internal signal.
    signal add_out2_r : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then

            -- Note that we no longer initialize out1 and out2 here, because
            -- they no longer correspond to registers.

            -- Initialize all signals that correspond to registers.
            in1_r      <= (others => '0');
            in2_r      <= (others => '0');
            in3_r      <= (others => '0');
            add_out1_r <= (others => '0');
            add_out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then

            in1_r <= in1;
            in2_r <= in2;

            -- Create the register for in3 (in3_r), which is required in this
            -- architecture because it does not connect to an output.
            in3_r <= in3;

            -- The add operation from the previous architectures is now stored
            -- in register add_out1_r. Even though add_out1_r connects to
            -- output out1, it also connects to add_out2_r, which requires an
            -- internal signal.
            -- 
            -- A common mistake is to assign the add to out1, which creates
            -- one register. However, there is no way to read from out1
            -- (in VHDL 1993) because it is an output. I often then see people
            -- converting out1 to inout or buffer, but do not do that as a
            -- workaround. That will synthesize to a different circuit, unless
            -- you know exactly what you are doing.
            --
            -- VHDL 2008 enables reading from outputs, which could eliminate
            -- this extra add_out1_r signal.
            add_out1_r <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));

            -- Create the second register on the output of the adder.
            add_out2_r <= add_out1_r;
        end if;
    end process;

    -- To avoid creating an additional register for out1, we assign out1 as a
    -- concurrent statement instead of on the rising clock edge.
    out1 <= add_out1_r;

    -- Similarly, we assign out2 as a concurrent statement to avoid an extra
    -- register.
    out2 <= std_logic_vector(unsigned(add_out2_r) + unsigned(in3_r));

-- COMMON MISTAKE: accidentally assigning out1 and out2 inside the process
-- is very common, which results in a different synthesized circuit. In the
-- best case, this mistake just adds extra registers. However, those extra
-- registers introduce timing delays which very often cause the rest of
-- your circuit to stop working.    
end ARCH3;


architecture ARCH3_2 of example is

    signal in1_r, in2_r : std_logic_vector(WIDTH-1 downto 0);
    signal in3_r        : std_logic_vector(WIDTH-1 downto 0);
    signal add_out1_r   : std_logic_vector(WIDTH-1 downto 0);
    signal add_out2_r   : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            in1_r      <= (others => '0');
            in2_r      <= (others => '0');
            in3_r      <= (others => '0');
            add_out1_r <= (others => '0');
            add_out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r      <= in1;
            in2_r      <= in2;
            in3_r      <= in3;
            add_out1_r <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
            add_out2_r <= add_out1_r;
        end if;
    end process;

    -- Since the code outside the process is all combinational logic, we can
    -- alternatively use any code that synthesizes to combinational logic.
    -- For example, we could have use a separate process as shown below, while
    -- making sure to follow all synthesis coding guidelines for combinational
    -- logic.
    --
    -- A process is overkill for this simple example, but if the combinational
    -- logic gets complex enough, it is quite common to have a process for
    -- sequential logic and a process for combinational logic. In fact, you
    -- could have multiple processes. For example, since out1 and out2 don't
    -- share any inputs, it might make sense to have a process for each. Again,
    -- for this simple example, that would be overkill, but if each output had
    -- complex logic, a process for each could be appropriate. When using
    -- many processes, it is a good idea to give them a meaningful label
    -- to make the purpose explicit. Also, this makes it easier to find
    -- variables in simulators because if you don't give a process a name,
    -- the tool with generator a random one for you. Searching through randomly
    -- named processes for a specific variable can be annoying.

    process(add_out1_r, add_out2_r, in3_r)
    begin
        out1 <= add_out1_r;
        out2 <= std_logic_vector(unsigned(add_out2_r) + unsigned(in3_r));
    end process;
    
end ARCH3_2;


-- The following architectures illustrates a slight simplification from
-- reading from an output, which is only possible in VHDL 2008. Uncomment
-- if you tool supports VHDL 2008.

--architecture ARCH3_2008 of example is

--    signal in1_r, in2_r : std_logic_vector(WIDTH-1 downto 0);
--    signal in3_r        : std_logic_vector(WIDTH-1 downto 0);

--    -- We no longer need add_out1_r in this version.
--    signal add_out2_r : std_logic_vector(WIDTH-1 downto 0);

--begin
--    process(clk, rst)
--    begin
--        if (rst = '1') then
--            in1_r      <= (others => '0');
--            in2_r      <= (others => '0');
--            in3_r      <= (others => '0');   
--            add_out2_r <= (others => '0');

--        elsif(rising_edge(clk)) then

--            in1_r <= in1;
--            in2_r <= in2;
--            in3_r <= in3;

--            -- In this version, we assign out1, since it is registered.
--            out1 <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));

--            -- VHDL 2008 allows us to read from outputs, so this functionality
--            -- replaces the need for the previous add_out1_r.
--            --
--            -- For this example, I personally prefer the previous architecture.
--            -- In the previous version the exact registers are more obvious.
--            -- However, they both synthesize to the same circuit.
--            add_out2_r <= out1;
--        end if;
--    end process;

--    out2 <= std_logic_vector(unsigned(add_out2_r) + unsigned(in3_r));

--end ARCH3_2008;


architecture ARCH4 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);

    -- These aren't necessary, but make the output registers more explicit.
    signal out1_r, out2_r : std_logic_vector(WIDTH-1 downto 0);

    -- Note that these signal do not have a _r suffix because they are not
    -- registered. We could potentially register the multiplier output and the
    -- slice it to form the output, but this version more closely matches the
    -- figure, which has a separate register on each output.
    signal mult_out : std_logic_vector(WIDTH*2-1 downto 0);
    signal add_out  : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            -- Note that we are only assigning registers here.
            in1_r  <= in1;
            in2_r  <= in2;
            in3_r  <= in3;
            out1_r <= mult_out(2*WIDTH-1 downto WIDTH);
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    -- Combinational logic.
    add_out  <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
    mult_out <= std_logic_vector(unsigned(add_out) * unsigned(in3_r));

    -- These aren't necessary, but are useful for consistently having a _r
    -- signal for every register. We could have just assigned out1 and out2
    -- inside the process.
    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH4;


-- This next architecture is a variation of the previous one that uses variables
-- create the combinational logic in the same process as the registers.

architecture ARCH4_2 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out1_r, out2_r      : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
        variable mult_out : std_logic_vector(WIDTH*2-1 downto 0);
        variable add_out  : std_logic_vector(WIDTH-1 downto 0);

    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r <= in1;
            in2_r <= in2;
            in3_r <= in3;

            -- We now use variables to create the combinational logic.
            -- In general, assigning a signal can become a wire or a register.
            -- In this case, they synthesize to wires as desired.
            --
            -- SYNTHESIS RULE: If there exists path where a variable is read
            -- before being written, it will become a register. This is similar
            -- the latch problem we had with combinational logic. If there is
            -- a path where the variable is read first, its value has to be
            -- preserved. With combinational logic, the synthesis tool had to
            -- add a latch to preserve the value. However, now we are doing the
            -- assignment on a rising edge, so synthesis creates a register,
            -- which is perfectly fine if you want a register. However, you
            -- might not want a register, which makes variables somewhat error
            -- prone. As a result, I suggest only ever using variables to
            -- implement wires. Use signals for all registers.
            --
            -- With this convention, every signal assignment should be to a
            -- signal with an _r suffix (a register), and every variable
            -- assigment should not contain an _r suffix because it is a wire.

            add_out  := std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
            mult_out := std_logic_vector(unsigned(add_out) * unsigned(in3_r));

            out1_r <= mult_out(2*WIDTH-1 downto WIDTH);
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH4_2;


architecture ARCH4_3 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out1_r, out2_r      : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
        variable mult_out : std_logic_vector(WIDTH*2-1 downto 0);

    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r <= in1;
            in2_r <= in2;
            in3_r <= in3;

            -- Here we get rid of the extra variable by just doing the add
            -- and multiply in the same statement.
            mult_out := std_logic_vector((unsigned(in1_r) + unsigned(in2_r)) * unsigned(in3_r));

            out1_r <= mult_out(2*WIDTH-1 downto WIDTH);
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH4_3;


architecture ARCH5 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out1_r, out2_r      : std_logic_vector(WIDTH-1 downto 0);
    signal mult_out            : std_logic_vector(WIDTH*2-1 downto 0);
    signal add_out             : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r  <= in1;
            in2_r  <= in2;
            in3_r  <= in3;
            out1_r <= add_out;

            -- This basically truncates the multiplication to the lower half.
            -- This might cause warnings in some synthesis tools, but there is
            -- no way around this to my knowledge.
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    -- Combinational logic.
    add_out  <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
    mult_out <= std_logic_vector(unsigned(add_out) * unsigned(in3_r));

    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH5;


-- Illustrates a similar implementation using variables.

architecture ARCH5_2 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out1_r, out2_r      : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
        variable mult_out : std_logic_vector(WIDTH*2-1 downto 0);
        variable add_out  : std_logic_vector(WIDTH-1 downto 0);

    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out1_r <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r <= in1;
            in2_r <= in2;
            in3_r <= in3;

            add_out  := std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
            mult_out := std_logic_vector(unsigned(add_out) * unsigned(in3_r));

            out1_r <= add_out;
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    out1 <= out1_r;
    out2 <= out2_r;
    
end ARCH5_2;


architecture ARCH6 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out2_r              : std_logic_vector(WIDTH-1 downto 0);

    signal add_out  : std_logic_vector(WIDTH-1 downto 0);
    signal mult_out : std_logic_vector(WIDTH*2-1 downto 0);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r  <= in1;
            in2_r  <= in2;
            in3_r  <= in3;
            out2_r <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    add_out  <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
    mult_out <= std_logic_vector(unsigned(add_out) * unsigned(in3_r));
    out1     <= add_out;
    out2     <= out2_r;
    
end ARCH6;


-- Illustrates a similar implementation using variables.

architecture ARCH6_2 of example is

    signal in1_r, in2_r, in3_r : std_logic_vector(WIDTH-1 downto 0);
    signal out2_r              : std_logic_vector(WIDTH-1 downto 0);
    signal add_out             : std_logic_vector(WIDTH-1 downto 0);
    
begin
    process(clk, rst)
        variable mult_out : std_logic_vector(WIDTH*2-1 downto 0);
        
    begin
        if (rst = '1') then
            in1_r  <= (others => '0');
            in2_r  <= (others => '0');
            in3_r  <= (others => '0');
            out2_r <= (others => '0');
            
        elsif(rising_edge(clk)) then
            in1_r <= in1;
            in2_r <= in2;
            in3_r <= in3;

            mult_out := std_logic_vector(unsigned(add_out) * unsigned(in3_r));
            out2_r   <= mult_out(WIDTH-1 downto 0);
        end if;
    end process;

    add_out <= std_logic_vector(unsigned(in1_r) + unsigned(in2_r));
    out1    <= add_out;
    out2    <= out2_r;
    
end ARCH6_2;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Top-level entity for evaluating all the examples above.

entity seq_example is
    generic(
        WIDTH : positive := 8);
    port (
        clk  : in  std_logic;
        rst  : in  std_logic;
        in1  : in  std_logic_vector(WIDTH-1 downto 0);
        in2  : in  std_logic_vector(WIDTH-1 downto 0);
        in3  : in  std_logic_vector(WIDTH-1 downto 0);
        out1 : out std_logic_vector(WIDTH-1 downto 0);
        out2 : out std_logic_vector(WIDTH-1 downto 0));
end seq_example;

architecture default_arch of seq_example is
begin

    U_SEQ_EXAMPLE : entity work.example(ARCH1)
        --U_SEQ_EXAMPLE: entity work.example(ARCH2)
        --U_SEQ_EXAMPLE: entity work.example(ARCH3)
        --U_SEQ_EXAMPLE: entity work.example(ARCH3_2)
        --U_SEQ_EXAMPLE: entity work.example(ARCH3_2008)
        --U_SEQ_EXAMPLE: entity work.example(ARCH4)
        --U_SEQ_EXAMPLE: entity work.example(ARCH4_2)
        --U_SEQ_EXAMPLE: entity work.example(ARCH4_3)
        --U_SEQ_EXAMPLE: entity work.example(ARCH5)
        --U_SEQ_EXAMPLE: entity work.example(ARCH5_2)
        --U_SEQ_EXAMPLE: entity work.example(ARCH6)
        --U_SEQ_EXAMPLE: entity work.example(ARCH6_2)      
        generic map (WIDTH => WIDTH)
        port map (
            clk  => clk,
            rst  => rst,
            in1  => in1,
            in2  => in2,
            in3  => in3,
            out1 => out1,
            out2 => out2
            );

end default_arch;


-- FINAL THOUGHTS:
-- When working with sequential logic, always figure out how many registers
-- you should have in our circuit before writing code. Then, simply declare
-- signals for each register using a _r suffix, and assign those signals on the
-- rising clock edge. Every assigned signal becomes a register, and the RHS of
-- each assignment becomes the input to that register.
--
-- If you need combinational logic that doesn't have a registered output, you
-- can move the code outside of the process for sequential logic by using
-- either concurrent statements, or a separate process(es) that follow my
-- coding guidelines for combinational logic.
--
-- If you want to use a single process for both sequential and combinational
-- logic, use variables for the combinational logic. However, make sure that
-- the variable is always assigned before it is read, or it will become a
-- register.
--
-- You can purposely use a variable for a register, but I don't recommend this
-- style.
