-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity add_tb is
end add_tb;


architecture exhaustive of add_tb is

    constant WIDTH : integer := 8;
    
    signal in0       : std_logic_vector(WIDTH-1 downto 0);
    signal in1       : std_logic_vector(WIDTH-1 downto 0);
    signal sum       : std_logic_vector(WIDTH-1 downto 0);
    signal carry_out : std_logic;

    procedure reference_model
        (
            in0       : in  std_logic_vector(WIDTH-1 downto 0);
            in1       : in  std_logic_vector(WIDTH-1 downto 0);
            sum       : out std_logic_vector(WIDTH-1 downto 0);
            carry_out : out std_logic) is

        variable temp : unsigned(WIDTH downto 0);
    begin
        temp      := resize(unsigned(in0), WIDTH+1) + unsigned(in1);
        sum       := std_logic_vector(temp(WIDTH-1 downto 0));
        carry_out := temp(WIDTH);
    end procedure;

begin  -- TB

    UUT : entity work.add
        generic map (WIDTH => WIDTH)
        port map (
            in0       => in0,
            in1       => in1,
            sum       => sum,
            carry_out => carry_out);

    process
        variable sum_correct       : std_logic_vector(WIDTH-1 downto 0);
        variable carry_out_correct : std_logic;
    begin
        -- Test all input combinations. Note that this is only feasible
        -- for small widths. For example, a 16-bit adder would have 32-inputs,
        -- which is 4 billion input combinations. Such a simulation would
        -- take a very long time. Better strategies for testbenches will be
        -- discussed later.
        
        for i in 0 to 2**WIDTH-1 loop
            for j in 0 to 2**WIDTH-1 loop
                
                in0 <= std_logic_vector(to_unsigned(i, WIDTH));
                in1 <= std_logic_vector(to_unsigned(j, WIDTH));
                wait for 10 ns;

                reference_model(in0, in1, sum_correct, carry_out_correct);
                assert(sum = sum_correct);
                assert(carry_out = carry_out_correct);

            end loop;  -- j
        end loop;  -- i
        wait;

    end process;

end exhaustive;
