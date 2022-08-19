-- Greg Stitt
-- University of Florida

-- https://electronics.stackexchange.com/questions/206949/vhdl-how-does-one-assign-custom-values-to-identifiers-of-an-enumerated-type

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

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
                -- Must cast in0 and in1 to either signed or unsigned because
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
                -- Same thing except using substraction.
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
                -- Doesn't define flags (causes latch)
                temp := signed(in0 and in1);

            when "11" =>
                -- Doesn't define flags (causes latch)
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
        -- are sequential statements. We technically aren't assigning a default
        -- value to the output signal, but we certainly can. In fact, if the
        -- ALU didn't support all possible select combinations, we could do that
        -- to avoid a latch on the output.
        --
        -- This strategy requires less code, tends to be less error prone, and
        -- guarantees you won't have latches. Use it whenever you can.
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
                temp   := signed(in0)+signed(in1);
                output <= std_logic_vector(temp);
                update_flags(temp);
                
            when C_SUB =>
                temp   := signed(in0)-signed(in1);
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
    -- You would probably never actually write code this way, but if you need
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


-- The following alu2 entity illustrates a more elegant approach to achieving
-- select values that are named in simualtion. It also demonstrates how to use
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



-- You might have wondered why we need the temp variable for this example.
-- In previous examples, we needed it to deal with carry signals or
-- multiplication. Here, we are ignoring the carry, so it doesn't serve that
-- purpose. We could have potentially just assigned the output signal, and then
-- read from that signal. However, VHDL 1993 does not support reading values
-- from outputs, which can be very annoying. You will see other suggestions
-- online, such as using inout or buffer instead of out, but I don't recommend
-- this unless you are very experienced with how synthesis treats these
-- constructs. When using VHDL 1993, any time you have an output that you also
-- need to read from, you need to create an internal signal/variable. I will
-- often do this by creating a signal with the same name, but an _s suffix.
--
-- Fortunately, VHDL 2008 eliminated this limitation, so you can read directly
-- from the output. However, output is a signal, so its value isn't updated
-- until the end of the process. So, we could potentially do something like
-- the following architecture, which updates the flags concurrently.


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

    --U_ALU : entity work.alu1(bad)
        --U_ALU : entity work.alu1(arch1)
        --U_ALU : entity work.alu1(arch2)
        --U_ALU : entity work.alu1(arch3)
        U_ALU : entity work.alu1(arch4)
        generic map (WIDTH => WIDTH)
        port map (in0    => in0,
                  in1    => in1,
                  sel    => sel,
                  neg    => neg,
                  pos    => pos,
                  zero   => zero,
                  output => output);


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
