-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity priority_encoder_tb is
end priority_encoder_tb;

architecture default_tb of priority_encoder_tb is

    constant NUM_INPUTS : integer := 8;
    constant NUM_OUTPUTS : integer := integer(ceil(log2(real(NUM_INPUTS))));
    
    signal inputs : std_logic_vector(NUM_INPUTS-1 downto 0);
    signal valid  : std_logic;
    signal result : std_logic_vector(NUM_OUTPUTS-1 downto 0);
    
begin

    UUT : entity work.priority_encoder(default_arch)
        generic map (NUM_INPUTS => NUM_INPUTS)
        port map (inputs => inputs,
                  valid  => valid,
                  result => result);

    process
        variable correct_result : std_logic_vector(NUM_OUTPUTS-1 downto 0);
        variable correct_valid  : std_logic;
    begin
        -- Test all input combinations.
        for i in 0 to 2**NUM_INPUTS-1 loop
            
            -- Assign the input
            inputs <= std_logic_vector(to_unsigned(i, NUM_INPUTS));
            wait for 10 ns;
            
            -- Simple reference model to get correct output.
            correct_result := (others => '0');
            for j in NUM_INPUTS-1 downto 0 loop
                if (inputs(j) = '1') then
                    correct_result := std_logic_vector(to_unsigned(j, NUM_OUTPUTS));
                    exit;
                end if;
            end loop;

            -- Reference model for correct valid.
            correct_valid := '0';
            if (inputs /= (NUM_INPUTS-1 downto 0 => '0')) then
                correct_valid := '1';
            end if;

            assert(result = correct_result);
            assert(valid = correct_valid);
            
        end loop;

        report "Tests completed.";
        wait;
        
    end process;    
end default_tb;
