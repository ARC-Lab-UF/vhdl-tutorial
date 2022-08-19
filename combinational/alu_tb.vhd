-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu_tb is
end alu_tb;

architecture exhaustive of alu_tb is

    constant WIDTH : integer := 8;

    signal in0            : std_logic_vector(WIDTH-1 downto 0);
    signal in1            : std_logic_vector(WIDTH-1 downto 0);
    signal sel            : std_logic_vector(1 downto 0);
    signal zero, pos, neg : std_logic;
    signal output         : std_logic_vector(WIDTH-1 downto 0);


    procedure reference_model
        (
            in0    : in  std_logic_vector(WIDTH-1 downto 0);
            in1    : in  std_logic_vector(WIDTH-1 downto 0);
            sel    : in  std_logic_vector(1 downto 0);
            neg    : out std_logic;
            pos    : out std_logic;
            zero   : out std_logic;
            output : out std_logic_vector(WIDTH-1 downto 0)) is

        variable temp : signed(WIDTH-1 downto 0);
    begin
        pos  := '0';
        neg  := '0';
        zero := '0';

        case sel is
            when "00"   => temp := signed(in0) + signed(in1);
            when "01"   => temp := signed(in0) - signed(in1);
            when "10"   => temp := signed(in0 and in1);
            when "11"   => temp := signed(in0 or in1);
            when others => null;
        end case;

        if (temp > 0) then pos     := '1';
        elsif (temp = 0) then zero := '1';
        else neg                   := '1';
        end if;

        output := std_logic_vector(temp);
    end procedure;

begin  -- TB

    UUT : entity work.alu
        generic map (WIDTH => WIDTH)
        port map (
            in0    => in0,
            in1    => in1,
            sel    => sel,
            neg    => neg,
            pos    => pos,
            zero   => zero,
            output => output);

    process
        variable output_correct                         : std_logic_vector(WIDTH-1 downto 0);
        variable neg_correct, pos_correct, zero_correct : std_logic;
    begin
        for i in 0 to 2**WIDTH-1 loop
            for j in 0 to 2**WIDTH-1 loop
                for k in 0 to (2**sel'length)-1 loop
                    
                    in0 <= std_logic_vector(to_unsigned(i, WIDTH));
                    in1 <= std_logic_vector(to_unsigned(j, WIDTH));
                    sel <= std_logic_vector(to_unsigned(k, 2));
                    wait for 10 ns;

                    reference_model(in0, in1, sel, neg_correct, pos_correct, zero_correct, output_correct);
                    assert(output = output_correct);
                    assert(neg = neg_correct or sel = "10" or sel = "11");
                    assert(pos = pos_correct or sel = "10" or sel = "11");
                    assert(zero = zero_correct or sel = "10" or sel = "11");

                end loop;  --k                
            end loop;  -- j
        end loop;  -- i

        report "Tests completed.";
        wait;

    end process;

end exhaustive;
