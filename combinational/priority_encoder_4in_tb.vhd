-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity priority_encoder_4in_tb is
end priority_encoder_4in_tb;

architecture default_tb of priority_encoder_4in_tb is

    signal inputs : std_logic_vector(3 downto 0);
    signal valid  : std_logic;
    signal result : std_logic_vector(1 downto 0);
    
begin

    UUT : entity work.priority_encoder_4in(default_arch)
        port map (inputs => inputs,
                  valid  => valid,
                  result => result);

    process
        variable correct_result : std_logic_vector(1 downto 0);
        variable correct_valid  : std_logic;
    begin
        -- Test all input combinations.
        for i in 0 to 15 loop
            
            -- Assign the input
            inputs <= std_logic_vector(to_unsigned(i, 4));
            wait for 10 ns;
            
            -- Simple reference model to get correct output.
            correct_result := (others => '0');
            for j in 3 downto 0 loop
                if (inputs(j) = '1') then
                    correct_result := std_logic_vector(to_unsigned(j, 2));
                    exit;
                end if;
            end loop;

            -- Reference model for correct valid.
            correct_valid := '0';
            if (inputs /= "0000") then
                correct_valid := '1';
            end if;

            assert(result = correct_result);
            assert(valid = correct_valid);
            
        end loop;

        report "Tests completed.";
        wait;
        
    end process;    
end default_tb;
