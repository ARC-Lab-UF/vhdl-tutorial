-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ripple_carry_adder_tb is
end ripple_carry_adder_tb;


architecture exhaustive of ripple_carry_adder_tb is

    constant WIDTH : integer := 8;

    signal x    : std_logic_vector(WIDTH-1 downto 0);
    signal y    : std_logic_vector(WIDTH-1 downto 0);
    signal cin  : std_logic;
    signal sum  : std_logic_vector(WIDTH-1 downto 0);
    signal cout : std_logic;

    procedure reference_model
        (
            x         : in  std_logic_vector(WIDTH-1 downto 0);
            y         : in  std_logic_vector(WIDTH-1 downto 0);
            carry_in  : in  std_logic;
            sum       : out std_logic_vector(WIDTH-1 downto 0);
            carry_out : out std_logic) is

        variable temp : unsigned(WIDTH downto 0);
    begin
        temp      := unsigned("0" & x) + unsigned(y) + (carry_in & "");
        sum       := std_logic_vector(temp(WIDTH-1 downto 0));
        carry_out := temp(WIDTH);
    end procedure;

begin  -- TB

    UUT : entity work.ripple_carry_adder
        generic map (WIDTH => WIDTH)
        port map (
            x    => x,
            y    => y,
            cin  => cin,
            sum  => sum,
            cout => cout);

    process
        variable sum_correct       : std_logic_vector(WIDTH-1 downto 0);
        variable carry_out_correct : std_logic;
    begin
        for i in 0 to 2**WIDTH-1 loop
            for j in 0 to 2**WIDTH-1 loop
                for k in 0 to 1 loop
                    
                    x                   <= std_logic_vector(to_unsigned(i, WIDTH));
                    y                   <= std_logic_vector(to_unsigned(j, WIDTH));
                    if (k = 0) then cin <= '0';
                    else cin            <= '1';
                    end if;
                    wait for 10 ns;

                    reference_model(x, y, cin, sum_correct, carry_out_correct);
                    assert(sum = sum_correct);
                    assert(cout = carry_out_correct);

                end loop;  -- k                
            end loop;  -- j
        end loop;  -- i
        wait;

        report "Tests completed.";
    end process;

end exhaustive;
