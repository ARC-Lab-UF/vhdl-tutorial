-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mult_tb is
end mult_tb;

architecture exhaustive of mult_tb is

    constant INPUT_WIDTH : integer := 8;
    
    signal in0       : std_logic_vector(INPUT_WIDTH-1 downto 0);
    signal in1       : std_logic_vector(INPUT_WIDTH-1 downto 0);
    signal product_signed    : std_logic_vector(INPUT_WIDTH*2-1 downto 0);
    signal product_unsigned    : std_logic_vector(INPUT_WIDTH*2-1 downto 0);  
    
begin  -- TB

    UUT_SIGNED : entity work.mult
        generic map (INPUT_WIDTH => INPUT_WIDTH,
                     IS_SIGNED => true)
        port map (
            in0       => in0,
            in1       => in1,
            product => product_signed);

    UUT_UNSIGNED : entity work.mult
        generic map (INPUT_WIDTH => INPUT_WIDTH,
                     IS_SIGNED => false)
        port map (
            in0       => in0,
            in1       => in1,
            product => product_unsigned);

    
    process
        variable product_signed_correct : std_logic_vector(INPUT_WIDTH*2-1 downto 0);
        variable product_unsigned_correct : std_logic_vector(INPUT_WIDTH*2-1 downto 0);
    begin
        -- Test all input combinations. Note that this is only feasible
        -- for small widths. Better strategies for testbenches will be
        -- discussed later.
        
        for i in 0 to 2**INPUT_WIDTH-1 loop
            for j in 0 to 2**INPUT_WIDTH-1 loop
                
                in0 <= std_logic_vector(to_unsigned(i, INPUT_WIDTH));
                in1 <= std_logic_vector(to_unsigned(j, INPUT_WIDTH));
                wait for 10 ns;

                product_signed_correct := std_logic_vector(signed(in0) * signed(in1));
                product_unsigned_correct := std_logic_vector(unsigned(in0) * unsigned(in1));
                
                assert(product_signed = product_signed_correct);
                assert(product_unsigned = product_unsigned_correct);
            end loop;  -- j
        end loop;  -- i

        report "Tests completed.";
        wait;
    end process;

end exhaustive;
