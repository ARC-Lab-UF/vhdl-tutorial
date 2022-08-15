-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux_2x1_tb is
end mux_2x1_tb;


architecture default of mux_2x1_tb is

  signal in1                : std_logic;
  signal in2                : std_logic;
  signal sel                : std_logic;
  signal output_with_select : std_logic;
  signal output_when_else   : std_logic;
  signal output_if          : std_logic;
  signal output_case        : std_logic;

begin  -- TB

  U_WITH_SELECT : entity work.mux_2x1(WITH_SELECT)
    port map (
      in1    => in1,
      in2    => in2,
      sel    => sel,
      output => output_with_select);

  U_WHEN_ELSE : entity work.mux_2x1(WHEN_ELSE)
    port map (
      in1    => in1,
      in2    => in2,
      sel    => sel,
      output => output_when_else);

  U_IF : entity work.mux_2x1(IF_STATEMENT)
    port map (
      in1    => in1,
      in2    => in2,
      sel    => sel,
      output => output_if);

  U_CASE : entity work.mux_2x1(CASE_STATEMENT)
    port map (
      in1    => in1,
      in2    => in2,
      sel    => sel,
      output => output_case);

  process
    variable temp : std_logic_vector(2 downto 0);

    function mux_test (
      signal in1 : std_logic;
      signal in2 : std_logic;
      signal sel : std_logic)
      return std_logic is
    begin  -- mux_test
      if (sel = '0') then
        return in1;
      else
        return in2;
      end if;
    end mux_test;

  begin
    -- test all input combinations
    for i in 0 to 7 loop
      temp := std_logic_vector(to_unsigned(i, 3));
      in1 <= temp(2);
      in2 <= temp(1);
      sel <= temp(0);
      wait for 10 ns;
      assert(output_with_select = mux_test(in1, in2, sel))
        report "Error : output_with_select incorrect for in1 = " & std_logic'image(in1) & " in2 = " & std_logic'image(in2) & " sel = " & std_logic'image(sel) severity warning;

      assert(output_when_else = mux_test(in1, in2, sel))
        report "Error : output_when_else incorrect for in1 = " & std_logic'image(in1) & " in2 = " & std_logic'image(in2) & " sel = " & std_logic'image(sel) severity warning;

      assert(output_if = mux_test(in1, in2, sel))
        report "Error : output_if incorrect for in1 = " & std_logic'image(in1) & " in2 = " & std_logic'image(in2) & " sel = " & std_logic'image(sel) severity warning;

      assert(output_case = mux_test(in1, in2, sel))
        report "Error : output_case incorrect for in1 = " & std_logic'image(in1) & " in2 = " & std_logic'image(in2) & " sel = " & std_logic'image(sel) severity warning;

    end loop;  -- i

    wait;

  end process;

end TB;
