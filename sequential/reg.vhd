-- Greg Stitt
-- University of Florida

-- This example illustrates a variety of different register entities. To
-- simulate/synthesize a different entity, change the comments in the reg
-- entity at the bottom of this file.
--
-- A full discussion of the tradeoffs of different reset types is outside the
-- scope of this example, but generally for FPGA designs I use asynchronous
-- resets because they are supported by the flip flops in every FPGA I use.
-- Synchronous resets still work, but require a mux in front of the flip flop
-- That mux takes away one input from the LUT in front of the flip flop, which
-- can cause significant increases in resource usage.
--
-- Also, as a general rule, I avoid resetting registers unless required. For
-- beginners I highly recommend resetting all registers, but once you are
-- comfortable with seeing 'U' values in a simulation, you can significantly
-- optimize a design by only resetting what is necessary. The reason for this
-- optimization is that reset signals have a huge fan-out, which makes it
-- challenging for placement and routing to create a good solution.

library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg_async_rst
-- Description: Implements a register with an active high, asynchronous reset.

entity reg_async_rst is
    generic(
        WIDTH : positive
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg_async_rst;

architecture BHV of reg_async_rst is
begin
    -- GUIDELINE 1 FOR SEQUENTIAL LOGIC: The sensitivity list should
    -- only have clock and reset (if there is an asynchronous reset). It
    -- technically shouldn't hurt anything if you include other signals, but
    -- they are not necessary, can slow down a simulation by needlessly
    -- retriggering the process, and might confuse a bad synthesis tool.
    
    process(clk, rst)
    begin
        -- SYNTHESIS GUIDELINE 2 FOR SEQUENTIAL LOGIC: All sequential logic with
        -- async reset should be described using the following basic structure:
        --
        -- if reset
        --   handle reset 
        -- elsif rising clock edge
        --   specify all non-reset functionality
        -- end if
        --
        -- Do not try to come up with another way of specifying equivalent
        -- behavior. Synthesis tools often require this template.
        --
        -- There are other techniques that will work with some tools, but since
        -- this template works for any form of sequential logic, there is no
        -- need to change it.
        
        if (rst = '1') then
            -- Reset the output to all 0s. The others statement sets all the
            -- bits equal to the specified value. It is part of a more general
            -- aggregation construct that will be discussed in other examples.
            output <= (others => '0');
            
        elsif (rising_edge(clk)) then

            -- SYNTHESIS RULE: Any assignment to a signal on a rising clock
            -- edge will be synthesized as a register, where the LHS is the
            -- output of the register, and the RHS is the input. This might
            -- seem obvious for this simple example, but it is critically
            -- important to remember this rule for bigger examples because
            -- adding or omitting a register is a very common source of bugs.
            -- When we "design the circuit" before writing the code, a huge
            -- part of that design is determining the number of registers.
            -- To ensure that the synthesized circuit has the same number of
            -- registers as our design circuit, we need to understand how
            -- registers get created during synthesis.
            --
            -- Note that variables can be synthesized as either wires or
            -- registers depending on the usage, which will be explained in
            -- later examples.
            output <= input;
        end if;
    end process;
end BHV;


library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg_sync_rst
-- Description: Implements a register with an active high, synchronous reset.

entity reg_sync_rst is
    generic(
        WIDTH : positive
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg_sync_rst;

architecture BHV of reg_sync_rst is
begin
    -- For a synchronous reset, we only need the clock in the sensitivity list.
    -- However, it doesn't really hurt to include reset because a simulation is
    -- unlikely to be frequently resetting a register, so it probably won't
    -- slow down the simulation much. Also, it won't affect synthesis. 
    process(clk)
    begin
        if (rising_edge(clk)) then
            -- For the synchronous reset, we check the reset on the rising
            -- clock edge.
            if (rst = '1') then
                output <= (others => '0');
            else
                output <= input;
            end if;
        end if;
    end process;
end BHV;


library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg_en_async_rst
-- Description: Implements a register with an enable, and an active high,
-- asynchronous reset. The register holds its value when enable isn't asserted.

entity reg_en_async_rst is
    generic(
        WIDTH : positive
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        en     : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg_en_async_rst;

architecture BHV of reg_en_async_rst is
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            output <= (others => '0');
            
        elsif (rising_edge(clk)) then
            -- Here we simply add an if statement to check if enable is
            -- asserted. Note that to comply with my coding guideline,
            -- this has to be done on the rising clock edge since it isn't
            -- related to reset.
            if (en = '1') then
                output <= input;
            end if;
        end if;
    end process;
end BHV;


library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg_en_sync_rst
-- Description: Implements a register with an enable, and an active high,
-- synchronous reset. The register holds its value when enable isn't asserted.

entity reg_en_sync_rst is
    generic(
        WIDTH : positive
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        en     : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg_en_sync_rst;


architecture BHV of reg_en_sync_rst is
begin
    process(clk, rst)
    begin
        if (rising_edge(clk)) then
            -- Here we move the reset on the clock edge to make it synchronous
            -- and then have an elsif for the enable. Note that we have to
            -- check the conditions in this order since reset has priority
            -- over the enable.
            if (rst = '1') then
                output <= (others => '0');
            elsif (en = '1') then
                output <= input;
            end if;
        end if;
    end process;
end BHV;


-- The following architecture is an example of a violation of synthesis coding
-- guidelines. It simulates perfectly, and is semantically correct in terms of
-- behavior, but it is unlikely that a synthesis tool will recognize this as
-- a register, at least without warnings.
--
-- I tested it in Quartus and it warned about en not being in the sensitivity
-- list, which suggests it thinks it is combinational logic. The RTL viewer
-- shows a register, so it might have worked, but is still a bad idea. I see
-- student code all the time that works in simulation but does not synthesize
-- correctly because of deviating from the specified template.
architecture BAD of reg_en_sync_rst is
begin
    process(clk, rst)
    begin
        if (rising_edge(clk) and rst = '1') then            
            output <= (others => '0');
        elsif (rising_edge(clk) and en = '1') then
            output <= input;            
        end if;
    end process;
end BAD;


library ieee;
use ieee.std_logic_1164.all;


-- Entity: reg_en_sync_rst
-- Description: Implements a highly parameterized register with configurable
-- width, reset type, reset activation level, and reset value.
--
-- As tempting as it might be to use this entity everywhere due to its
-- versatility, it quickly becomes problematic structurally instantiating a
-- register everywhere you need one. I almost never do this unless I know
-- I need the versatility, which is very rare.
--
-- Generally, since creating a register only requires a single assignment
-- on a rising clock edge, I'll just add an assignment to existing code to
-- create more registers. That might lack the flexibility, but it is rare that
-- I need to change the activation level or type of reset. For example, for
-- FPGA circuits, asynchronous reset tend to use less resources, which makes
-- it my default reset type.

entity reg_flexible is
    generic(
        WIDTH                  : positive;
        HAS_ASYNC_RESET        : boolean          := true;
        RESET_ACTIVATION_LEVEL : std_logic        := '1';

        -- Here we use an unconstrained vector to work around an annoying
        -- limitation of VHDL (pre-2008). Ideally, we would just do something
        -- like this:
        --
        -- RESET_VALUE : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
        --
        -- However, the 1993 version of VHDL does not allow use of generics
        -- to define other generics. Similarly, we can't do:
        --
        -- RESET_VALUE : std_logic_vector := (WIDTH-1 downto 0 => '0');
        --
        -- Using an unconstrained vector is a workaround because it will get
        -- its width from whatever value we pass in the generic map.
        --
        -- We use default value of an empty vector so that the user does not
        -- have to specify a reset value, since 99% of the time, it will just
        -- be all 0s. Below, you will see how we simply check for the empty
        -- vector, and then use all 0s.

        RESET_VALUE            : std_logic_vector := ""
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        en     : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg_flexible;


-- In the first architecture for the flexible register, we use if generates
-- to handle the two reset types.
architecture BHV1 of reg_flexible is
begin
    
    ASYNC_RESET : if HAS_ASYNC_RESET generate     
        process(clk, rst)
        begin
            -- Here I check the reset level generic instead of a hardcoded
            -- literal.
            if (rst = RESET_ACTIVATION_LEVEL) then
                -- Because I assigned an empty vector as a default value for
                -- the reset value, I can check if it is empty to see whether
                -- or not to use a user-specified value, or the more common 0
                -- value.
                --
                -- Note that ideally I would have just made the default value
                -- WIDTH '0's, but there is no way to do this in VHDL pre-2008
                -- because you can't use a generic value (e.g. WIDTH) to
                -- define defalut values for other generics.
                --
                -- Using the empty vector as a default is a little ugly, but
                -- provides a workaround.
                
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
    end generate ASYNC_RESET;
    
    SYNC_RESET : if not HAS_ASYNC_RESET generate
        process(clk)
        begin
            if (rising_edge(clk)) then
                if (rst = RESET_ACTIVATION_LEVEL) then
                    if (RESET_VALUE /= "") then
                        output <= RESET_VALUE;
                    else
                        output <= (others => '0');
                    end if;
                elsif (en = '1') then
                    output <= input;
                end if;
            end if;
        end process;
    end generate;
end BHV1;



-- In the second architecture we use a single process to describe both
-- reset types.

architecture BHV2 of reg_flexible is
begin
    -- We include clock and reset since we have to support the asynchronous
    -- reset. This isn't ideal for the synchronous reset, but will still
    -- function correctly in simulation and synthesis.
    process(clk, rst)
    begin
        -- First we check to see if the register has an async reset. This might
        -- seem like a violation of my coding guidelines, but it isn't because
        -- HAS_ASYNC_RESET is essentially a constant. If it is '1', this is
        -- equivalent to "if (rst)". If it is '0', the synthesis tool will
        -- perform dead-code elimination and get rid of the if completely.
        if (HAS_ASYNC_RESET and rst = RESET_ACTIVATION_LEVEL) then         
            if (RESET_VALUE /= "") then
                output <= RESET_VALUE;
            else
                output <= (others => '0');
            end if;
        elsif (rising_edge(clk)) then
            if (not HAS_ASYNC_RESET and rst = RESET_ACTIVATION_LEVEL) then
                if (RESET_VALUE /= "") then
                    output <= RESET_VALUE;
                else
                    output <= (others => '0');
                end if;
            elsif (en = '1') then
                output <= input;
            end if;
        end if;
    end process;
end BHV2;


library ieee;
use ieee.std_logic_1164.all;

-- Entity: reg
-- Description: Provides a top-level reigster entity for evaluating all the
-- implementations shown above.

entity reg is
    generic(
        WIDTH : positive := 8
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        en     : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg;

architecture default_arch of reg is
begin

    -- INSTRUCTIONS: Uncomment the architecture/entity that you want to
    -- evaluate.

    U_REG : entity work.reg_en_async_rst
    --U_REG : entity work.reg_en_sync_rst(BHV)
    --U_REG : entity work.reg_en_sync_rst(BAD)
        generic map (WIDTH => WIDTH)
        port map (clk    => clk,
                  rst    => rst,
                  en     => en,
                  input  => input,
                  output => output);

    -- Feel free to test these, but note that the testbench assumes an
    -- active high reset, so some parameter combinations might have failed
    -- assertions.

    --U_REG : entity work.reg_flexible(BHV1)
    --    --U_REG : entity work.reg_flexible(BHV2)  
    --    generic map (
    --        WIDTH => WIDTH,
    --        HAS_ASYNC_RESET => true,
    --        RESET_ACTIVATION_LEVEL => '1',
            
    --        -- This is a convenient way of specifying all 0s for a known range.
    --        -- The usual (others => '0') will not work here because RESET_VALUE
    --        -- is unconstrained.
    --        RESET_VALUE => (WIDTH-1 downto 0 => '0')
    --        )
    --    port map (clk    => clk,
    --              rst    => rst,
    --              en     => en,
    --              input  => input,
    --              output => output);

    
end default_arch;
