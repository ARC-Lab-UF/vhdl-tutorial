-- Greg Stitt
-- University of Florida

-- In this example, we look at various constructs that can be used to create
-- an ALU. One of the most important takeaway points is to avoid latches by
-- ensuring that all outputs are defined on all paths through a process.
--
-- Like the other examples, there are different architectures and entities,
-- along with the top-level alu entity at the bottom of the file, which can be
-- change to synthesize or simulate each implementation.
--
-- NOTE: Simulation of some of these implemenations may result in warnings
-- similar to the following:
--
-- Warning: NUMERIC_STD.">": metavalue detected, returning FALSE
-- #    Time: 0 ns  Iteration: 0  Instance: /alu_tb/UUT/U_ALU
--
-- A metavalue is anything other than '1' or '0' (e.g., 'X', '-', 'U').
-- As long as this happens at time 0, it can be ignored. It usually happens
-- because a 'U' reaches the input of an operator (in this case >). It can
-- also happen at other times, in which case you should verify that it is
-- intended. Explicit use of don't cares can cause this warning, as is discussed
-- in the example below.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Entity: alu1
-- ALU with generic width that does addition (sel = "00"),
-- subtraction (sel="01"), and (sel="10"), or (sel="11"). There are also
-- status flags to signify positive, negative, and zero results.
-- For illustrative purposes, assume the flags can be set to any value for
-- the "and" and "or" cases.

entity alu1 is
    generic (
        WIDTH : positive := 8);
    port (
        in0    : in  std_logic_vector(WIDTH-1 downto 0);
        in1    : in  std_logic_vector(WIDTH-1 downto 0);
        sel    : in  std_logic_vector(1 downto 0);
        neg    : out std_logic;
        pos    : out std_logic;
        zero   : out std_logic;
        output : out std_logic_vector(WIDTH-1 downto 0));
end alu1;


-- Although this architecture will simulate correctly, there is a synthesis
-- problem. Note that the status flags are not defined by the "and" or "or"
-- cases because the ALU's flags do not have meaning for these operations.
-- However, we are describing combinational logic. What happens to the flags
-- if the "and" or "or" cases are selected according to this code?
-- The answer is that flags preserve their previous values.
-- Using combinational logic, how can you preserve a value? In general,
-- you can't, which is the purpose of sequential logic. So, what ends up
-- happening is that the synthesis tool will infer latches because there is no
-- clock signal to create a register. Although this could technically still
-- work, your resulting circuit is not what was intended and wastes resources.
-- In addition, latches can cause many timing problems unless you know how to
-- properly use them. They are especially problematic in FPGAs because there
-- are no latches, so the synthesis tools must imitate their behavior.
-- In general, you sohuld always avoid latches in combinational logic. If you
-- intentionally want a latch somewhere, design your circuit first with the
-- latch, then write the code for it. For FPGA designs, latches should almost
-- always be avoided, unless you have a very good reason to use them.
--
-- To solve this problem, every path through the process has to define each
-- output and signal. If *any* path exists that does not define an output, that
-- output will be inferred as a latch. In this example, the paths through the
-- "and" and "or" cases do not define the flag outputs, and therefore result
-- in latches.
--
-- SYNTHESIS GUIDELINE 2 FOR COMBINATIONAL LOGIC: Make sure that every path
-- through the process defines every output and internal signal.

architecture bad of alu1 is
begin
    -- Remember to use combinational synthesis guideline 1 (put all inputs in
    -- sensitivity list)
    
    process(in0, in1, sel)
        variable temp : signed(WIDTH-1 downto 0);
    begin
        case sel is
            when "00" =>
                -- Must cast in0 and in1 to either signed because
                -- addition is not defined for std_logic_vector when using
                -- ieee.numeric_std  
                temp := signed(in0) + signed(in1);

                -- Set the flags.
                if (temp > 0) then
                    pos  <= '1';
                    neg  <= '0';
                    zero <= '0';
                elsif (temp = 0) then
                    pos  <= '0';
                    neg  <= '0';
                    zero <= '1';
                else
                    pos  <= '0';
                    neg  <= '1';
                    zero <= '0';
                end if;

            when "01" =>
                -- Same thing except using subtraction.
                temp := signed(in0) - signed(in1);

                if (temp > 0) then
                    pos  <= '1';
                    neg  <= '0';
                    zero <= '0';
                elsif (temp = 0) then
                    pos  <= '0';
                    neg  <= '0';
                    zero <= '1';
                else
                    pos  <= '0';
                    neg  <= '1';
                    zero <= '0';
                end if;

            when "10" =>
                -- Doesn't define flags (causes latches on each flag)
                temp := signed(in0 and in1);

            when "11" =>
                -- Doesn't define flags (causes latch on each flag)
                temp := signed(in0 or in1);

            when others => null;
        end case;

        output <= std_logic_vector(temp);

    end process;
end bad;


-- This architecture demonstrates one possible way of fixing the latch issue.

architecture arch1 of alu1 is
begin  -- arch1

    process(in0, in1, sel)
        variable temp : signed(WIDTH-1 downto 0);
    begin
        case sel is
            when "00" =>
                temp := signed(in0) + signed(in1);

            when "01" =>
                temp := signed(in0) - signed(in1);

            when "10" =>
                temp := signed(in0 and in1);

            when "11" =>
                temp := signed(in0 or in1);

            when others => null;
        end case;

        -- By moving the flag definitions outside of the case statement, we
        -- guarantee that all the flags are defined by all paths.

        if (temp > 0) then
            pos  <= '1';
            neg  <= '0';
            zero <= '0';
        elsif (temp = 0) then
            pos  <= '0';
            neg  <= '0';
            zero <= '1';
        else
            pos  <= '0';
            neg  <= '1';
            zero <= '0';
        end if;

        output <= std_logic_vector(temp);

    end process;
end arch1;


architecture arch2 of alu1 is
begin  -- arch2

    process(in0, in1, sel)
        variable temp : signed(WIDTH-1 downto 0);
    begin
        -- Another option that I recommend in most cases is to assign default
        -- values to each output at the beginning of the process. Then, if
        -- you need to update the value later, you still can because these
        -- are sequential statements. We aren't assigning a default
        -- value to the output signal, but we certainly can. In fact, if the
        -- ALU didn't support all possible select combinations, we could do that
        -- to avoid a latch on the output.
        --
        -- This strategy requires less code, tends to be less error prone, and
        -- guarantees you won't have latches on the signals with default values.
        -- Use it whenever you can.
        --
        -- Note that this approach doesn't work with concurrent assignements.
        -- Multiple concurrent assignments to the same signal creates multiple
        -- drivers, which will result in synthesis errors. I haven't looked
        -- into the exact simulation semantics of multiple drivers, but in
        -- Modelsim, you will get undefined values from multiple drivers.
        
        pos  <= '0';
        neg  <= '0';
        zero <= '0';

        case sel is
            when "00" =>
                temp := signed(in0) + signed(in1);

            when "01" =>
                temp := signed(in0) - signed(in1);

            when "10" =>
                temp := signed(in0 and in1);

            when "11" =>
                temp := signed(in0 or in1);

            when others => null;
        end case;

        -- This code is simpler now that we have default values of '0'.
        if (temp > 0) then
            pos <= '1';
        elsif (temp = 0) then
            zero <= '1';
        else
            neg <= '1';
        end if;

        output <= std_logic_vector(temp);
    end process;
end arch2;


architecture arch3 of alu1 is

    -- We can use constants to make the case statements more readable, which
    -- would be very useful for a large number of ALU operations.
    constant C_ADD : std_logic_vector(1 downto 0) := "00";
    constant C_SUB : std_logic_vector(1 downto 0) := "01";
    constant C_AND : std_logic_vector(1 downto 0) := "10";
    constant C_OR  : std_logic_vector(1 downto 0) := "11";

begin  -- arch3

    process(in0, in1, sel)
        variable temp : signed(WIDTH-1 downto 0);

        -- Here we use a procedure (similar to a function call) to perform
        -- the repeated functionality of setting the output flags.
        procedure update_flags(variable result : in signed(WIDTH-1 downto 0)) is
        begin
            pos  <= '0';
            neg  <= '0';
            zero <= '0';

            if (result > 0) then pos     <= '1';
            elsif (result = 0) then zero <= '1';
            else neg                     <= '1';
            end if;
        end update_flags;
        
    begin
        -- If we really don't care what happens for the and and or operations,
        -- we can make the default an actual don't care '-', which might give
        -- the synthesis tool the ability to do extra logic minimization.
        --
        -- In general, I avoid assigning don't cares unless I am specifically
        -- trying to optimize an entity because don't care values reaching
        -- arithmetic operators can cause a huge number of simulation warnings.
        --
        -- Another option would be to add a boolean generic that specifies
        -- whether or not to use don't cares so that you can turn it off for
        -- simulations.
        pos  <= '-';
        neg  <= '-';
        zero <= '-';

        case sel is
            when C_ADD =>
                temp   := signed(in0) + signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when C_SUB =>
                temp   := signed(in0) - signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when C_AND =>
                output <= in0 and in1;

            when C_OR =>
                output <= in0 or in1;
                
            when others => null;
        end case;
    end process;
end arch3;


-- One disadvantage to using constants for the select is that the simulation
-- will show the values of the select. Although that isn't a problem with
-- just 4 selects, it would be confusing for 16+ select values. Ideally, we
-- could see the name of the constant so we don't have to remember the encoded
-- value.
--
-- This goal can be achieved via enumerated types. The following architecture
-- isn't a great way of doing this, and instead just illustrates how it could
-- be done for this entity. The following entity shows a more elegant way of
-- accomplishing this goal.

architecture arch4 of alu1 is

    constant C_ADD : std_logic_vector(1 downto 0) := "00";
    constant C_SUB : std_logic_vector(1 downto 0) := "01";
    constant C_AND : std_logic_vector(1 downto 0) := "10";
    constant C_OR  : std_logic_vector(1 downto 0) := "11";

    -- Create an enumerated type for the select values.
    type alu_sel_t is (ADD_SEL, SUB_SEL, AND_SEL, OR_SEL);

    -- Create a new select signal using that type.
    signal alu_sel : alu_sel_t;
    
begin  -- arch4

    -- We have to manually convert between std_logic_vector and alu_sel_t.
    -- You would probably never write ALU code this way, but if you need
    -- to do this conversion, this is how to do it.
    
    alu_sel <= ADD_SEL when sel = C_ADD else
               SUB_SEL when sel = C_SUB else
               AND_SEL when sel = C_AND else
               OR_SEL;

    -- We now use the alu_sel in the sensitivity list instead of sel.
    process(in0, in1, alu_sel)
        variable temp : signed(WIDTH-1 downto 0);

        procedure update_flags(variable result : in signed(WIDTH-1 downto 0)) is
        begin
            pos  <= '0';
            neg  <= '0';
            zero <= '0';

            if (result > 0) then pos     <= '1';
            elsif (result = 0) then zero <= '1';
            else neg                     <= '1';
            end if;
        end update_flags;
        
    begin
        pos  <= '-';
        neg  <= '-';
        zero <= '-';

        -- In this updated case, we use the enumerated type names, which will
        -- show up in simulation.
        case alu_sel is
            when ADD_SEL =>
                temp   := signed(in0) + signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when SUB_SEL =>
                temp   := signed(in0) - signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when AND_SEL =>
                output <= in0 and in1;

            when OR_SEL =>
                output <= in0 or in1;
                
            when others => null;
        end case;
    end process;

-- Another alternative we could have done would be to leave the previous
-- process using sel instead of alu_sel. In this case, we can see the names
-- of the select values by adding alu_sel to the waveform, while using the
-- sel signal in the case statement. However, that approach would likely
-- lead to synthesis warnings because alu_sel has no impact on any outputs.
end arch4;


-- In this architecture, we separate the output and flag logic so that
-- the process only defines the output.

architecture arch5 of alu1 is

    constant C_ADD : std_logic_vector(1 downto 0) := "00";
    constant C_SUB : std_logic_vector(1 downto 0) := "01";
    constant C_AND : std_logic_vector(1 downto 0) := "10";
    constant C_OR  : std_logic_vector(1 downto 0) := "11";

    -- In this architecture, we can no longer use a variable because a
    -- variable's scope is limited to a process. So, we need an extra signal
    -- for the output. You might be tempted to just write to the output from
    -- the process, and then read from it using concurrent statements. However,
    -- that does not work pre-2008. Whenever I need to read from an output,
    -- I just make a signal with the same name, and a _s suffix, which
    -- signifies the purpose is just an extra signal to enable reading from the
    -- output.
    signal output_s : std_logic_vector(output'range);
    
begin

    process(in0, in1, sel)
    begin
        case sel is
            when C_ADD =>
                output_s <= std_logic_vector(signed(in0) + signed(in1));
                
            when C_SUB =>
                output_s <= std_logic_vector(signed(in0) - signed(in1));
                
            when C_AND =>
                output_s <= in0 and in1;

            when C_OR =>
                output_s <= in0 or in1;
                
            when others => null;
        end case;
    end process;

    pos  <= '1' when signed(output_s) > 0 else '0';
    neg  <= '1' when signed(output_s) < 0 else '0';
    zero <= '1' when signed(output_s) = 0 else '0';

    output <= output_s;
    
end arch5;


-- In this architecture, we make everything concurrent statements.

architecture arch6 of alu1 is

    constant C_ADD : std_logic_vector(1 downto 0) := "00";
    constant C_SUB : std_logic_vector(1 downto 0) := "01";
    constant C_AND : std_logic_vector(1 downto 0) := "10";
    constant C_OR  : std_logic_vector(1 downto 0) := "11";

    signal output_s : std_logic_vector(output'range);
    
begin

    with sel select
        output_s <= std_logic_vector(signed(in0) + signed(in1)) when C_ADD,
        std_logic_vector(signed(in0) - signed(in1))             when C_SUB,
        in0 and in1                                             when C_AND,
        in0 or in1                                              when others;

    pos  <= '1' when signed(output_s) > 0 else '0';
    neg  <= '1' when signed(output_s) < 0 else '0';
    zero <= '1' when signed(output_s) = 0 else '0';

    output <= output_s;
    
end arch6;


-- In VHDL 2008, you can read from outputs, so we no longer need the output_s
-- signal.

--architecture arch6_2008 of alu1 is

--    constant C_ADD : std_logic_vector(1 downto 0) := "00";
--    constant C_SUB : std_logic_vector(1 downto 0) := "01";
--    constant C_AND : std_logic_vector(1 downto 0) := "10";
--    constant C_OR  : std_logic_vector(1 downto 0) := "11";
    
--begin

--    with sel select
--        output <= std_logic_vector(signed(in0) + signed(in1)) when C_ADD,
--        std_logic_vector(signed(in0) - signed(in1))           when C_SUB,
--        in0 and in1                                           when C_AND,
--        in0 or in1                                            when others;

--    pos  <= '1' when signed(output) > 0 else '0';
--    neg  <= '1' when signed(output) < 0 else '0';
--    zero <= '1' when signed(output) = 0 else '0';
    
--end arch6_2008;


--------------------------------------------------------------------------

-- The following alu2 entity illustrates a more elegant approach to achieving
-- select values that are named in simulation. It also demonstrates how to use
-- a package.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- See alu_pkg.vhd for the contents of the package. Basically, the package
-- defines the alu_sel_t type so that we can use it in the port.
use work.alu_pkg.all;

entity alu2 is
    generic (
        WIDTH : positive := 8);
    port (
        in0    : in  std_logic_vector(WIDTH-1 downto 0);
        in1    : in  std_logic_vector(WIDTH-1 downto 0);
        -- Since we defined alu_sel_t in alu_pkg, we can use a custom type
        -- in the port of this entity.
        sel    : in  alu_sel_t;
        neg    : out std_logic;
        pos    : out std_logic;
        zero   : out std_logic;
        output : out std_logic_vector(WIDTH-1 downto 0));
end alu2;

architecture arch1 of alu2 is
begin
    -- Since we made sel use the alu_sel_t, we don't have to change anything
    -- here other than the names of the constants.
    process(in0, in1, sel)
        variable temp : signed(WIDTH-1 downto 0);

        procedure update_flags(variable result : in signed(WIDTH-1 downto 0)) is
        begin
            pos  <= '0';
            neg  <= '0';
            zero <= '0';

            if (result > 0) then pos     <= '1';
            elsif (result = 0) then zero <= '1';
            else neg                     <= '1';
            end if;
        end update_flags;
        
    begin
        pos  <= '-';
        neg  <= '-';
        zero <= '-';

        -- This case uses ADD_SEL instead of C_ADD, etc.
        case sel is
            when ADD_SEL =>
                temp   := signed(in0) + signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when SUB_SEL =>
                temp   := signed(in0) - signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when AND_SEL =>
                output <= in0 and in1;

            when OR_SEL =>
                output <= in0 or in1;
                
            when others => null;
        end case;
    end process;
end arch1;

------------------------------------------------------------------------
-- Top level ALU entity used for synthesis and simulation.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.alu_pkg.all;

entity alu is
    generic (
        WIDTH : positive := 8);
    port (
        in0    : in  std_logic_vector(WIDTH-1 downto 0);
        in1    : in  std_logic_vector(WIDTH-1 downto 0);
        sel    : in  std_logic_vector(1 downto 0);
        neg    : out std_logic;
        pos    : out std_logic;
        zero   : out std_logic;
        output : out std_logic_vector(WIDTH-1 downto 0));
end alu;

architecture default_arch of alu is

    signal alu_sel : alu_sel_t;
begin

    -- INSTRUCTIONS: uncomment the architecture and/or entity that you want
    -- to evaluate.

    --U_ALU : entity work.alu1(bad)
    U_ALU : entity work.alu1(arch1)
        --U_ALU : entity work.alu1(arch2)
        --U_ALU : entity work.alu1(arch3)
        --U_ALU : entity work.alu1(arch4)
        --U_ALU : entity work.alu1(arch5)
        --U_ALU : entity work.alu1(arch6)
        --U_ALU : entity work.alu1(arch6_2008)
        generic map (WIDTH => WIDTH)
        port map (in0    => in0,
                  in1    => in1,
                  sel    => sel,
                  neg    => neg,
                  pos    => pos,
                  zero   => zero,
                  output => output);

    ---------------------------------------------------------
    -- Uncomment everything below if using the alu2 entity. Make sure to
    -- comment out everything above also.

    --alu_sel <= ADD_SEL when sel = ADD_SEL'encoding else
    --           SUB_SEL when sel = SUB_SEL'encoding else
    --           AND_SEL when sel = AND_SEL'encoding else
    --           OR_SEL;

    --U_ALU : entity work.alu2(arch1)
    --    generic map (WIDTH => WIDTH)
    --    port map (in0    => in0,
    --              in1    => in1,
    --              sel    => alu_sel,
    --              neg    => neg,
    --              pos    => pos,
    --              zero   => zero,
    --              output => output);


end default_arch;
