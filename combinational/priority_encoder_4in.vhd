-- Greg Stitt
-- University of Florida

-- Entity: priority_encoder_4in_if
-- Description: implements a 4 input (2 output) priority encoder with a valid
-- output that is asserted when any of the inputs are asserted.

entity priority_encoder_4in is
    port (
        inputs : in  std_logic_vector(3 downto 0);
        valid  : out std_logic;
        result : out std_logic_vector(1 downto 0)
        );      
end priority_encoder_4in;


-- We first use a behavioral architecture with an if statement to define the
-- behavior of the priority encoder.

architecture IF_STATEMENT of priority_encoder_4in is
begin
    process(inputs)
    begin
        if (inputs(3) = '1') then
            result <= "11";
            valid  <= '1';
        elsif (inputs(2) = '1') then
            result <= "10";
            valid  <= '1';
        elsif (inputs(1) = '1') then
            result <= "01";
            valid  <= '1';
        elsif (inputs(0) = '1') then
            result <= "00";
            valid  <= '1';
        else
            result <= "00";
            valid  <= '0';
        end if;
    end process;
    
end IF_STATEMENT;


-- This architecture is similar, but takes advantage of sequential statements
-- to initially assign a default value of '1' to the valid output. Assigning
-- default values for combinational-logic outputs is a good design practice
-- to reduce code size, and more importantly, eliminate the potential for
-- latches, which will be explained in a later example.

architecture IF_STATEMENT2 of priority_encoder_4in is
begin
    process(inputs)
    begin
        valid <= '1';

        if (inputs(3) = '1') then
            result <= "11";
        elsif (inputs(2) = '1') then
            result <= "10";
        elsif (inputs(1) = '1') then
            result <= "01";
        elsif (inputs(0) = '1') then
            result <= "00";
        else
            valid  <= '0';
            result <= "00";
        end if;
    end process;
    
end IF_STATEMENT2;


-- This architecture uses a case statement instead of an if.
-- It demonstrates that when a circuit has some notion of priority, an
-- if statement is generally a more appropriate construct.

-- SYNTHESIS INSIGHTS: For synthesis, a case statement will usually become a
-- mux, where each of the when clauses correspond to a select value. If
-- statements can become either a mux or a priority encoder (plus other logic),
-- depending on the specific conditions. In the situation when only one
-- condition can be true across and if-elsif chain, the if statement will
-- synthesize to a mux, which is what we saw in the mux example. For a mux,
-- when checked the select value with if-elsif statements. A select can only
-- be one possible value, so that example synthesized into the desired mux.
-- For the priority encoder, any of the input bits can be asserted at any time,
-- so synthesis creates the desired priority encoder.

architecture CASE_STATEMENT of priority_encoder_4in is
begin
    process(inputs)
    begin
        valid <= '1';

        case (inputs) is
            when "0000" => result <= "00"; valid <= '0';
            when "0001" => result <= "00";
            when "0010" => result <= "01";
            when "0011" => result <= "01";
            when "0100" => result <= "10";
            when "0101" => result <= "10";
            when "0110" => result <= "10";
            when "0111" => result <= "10";
            when others => result <= "11";
        end case;
    end process;
    
end CASE_STATEMENT;


-- Here is a shorter way of using the case statement by specifying multiple
-- when values on the same line.

architecture CASE_STATEMENT2 of priority_encoder_4in is
begin
    process(inputs)
    begin
        valid <= '1';

        case (inputs) is
            when "0000"                            => result <= "00"; valid <= '0';
            when "0001"                            => result <= "00";
            when "0010" | "0011"                   => result <= "01";
            when "0100" | "0101" | "0110" | "0111" => result <= "10";
            when others                            => result <= "11";
        end case;
    end process;
end CASE_STATEMENT2;


-- One commonly attempted approach with case statements is to use the std_logic
-- don't care '-' to simplify case statements. Unfortunately, this does work
-- in VHDL standards before 2008. The reason it doesn't work is that '-' is an
-- exlicit value. Instead of meaning '0' or '1', an input will only match a '-'
-- if the input is also explicitly '-'. 

architecture CASE_STATEMENT_BAD of priority_encoder_4in is
begin
    process(inputs)
    begin
        valid <= '1';

        case (inputs) is
            when "1---" => result <= "11";
            when "01--" => result <= "10";
            when "001-" => result <= "01";
            when "0001" => result <= "00";
            when others => result <= "00"; valid <= '0';
        end case;
    end process;
end CASE_STATEMENT_BAD;


-- VHDL 2008 introduced the case? construct, which revises how '-' works so that
-- you can use it to represent '0' or '1'. However, VHDL 2008 is not widely
-- supported, so I would not recommend this construct if you want your code
-- to work with every tool.

architecture CASE_STATEMENT_2008 of priority_encoder_4in is
begin
    process(inputs)
    begin
        valid <= '1';

        case? (inputs) is
            when "1---" => result <= "11";
            when "01--" => result <= "10";
            when "001-" => result <= "01";
            when "0001" => result <= "00";
            when others => result <= "00"; valid <= '0';
        end case?;
    end process;
end CASE_STATEMENT_2008;
