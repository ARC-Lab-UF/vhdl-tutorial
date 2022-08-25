-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity bit_diff_tb is
end bit_diff_tb;

architecture random_tb of bit_diff_tb is

    constant WIDTH     : integer := 16;
    constant NUM_TESTS : integer := 10000;

    signal clk_en         : std_logic := '1';
    signal clk            : std_logic := '0';
    signal rst            : std_logic;
    signal go             : std_logic;
    signal data           : std_logic_vector(WIDTH-1 downto 0);
    signal result         : std_logic_vector(WIDTH-1 downto 0);
    signal done           : std_logic;

    signal check_done_low : std_logic;
        
    function model(d : std_logic_vector(WIDTH-1 downto 0)) return signed is
        variable diff : signed(WIDTH-1 downto 0) := (others => '0');
        variable data : unsigned(WIDTH-1 downto 0);
    begin
        data := unsigned(d);

        for i in 0 to WIDTH-1 loop
            if (data(0) = '1') then
                diff := diff + 1;
            else
                diff := diff - 1;
            end if;

            data := shift_right(data, 1);
        end loop;

        return diff;
    end function;
    
begin
    
    DUT : entity work.bit_diff
        generic map (WIDTH => WIDTH)
        port map (
            clk    => clk,
            rst    => rst,
            go     => go,
            data   => data,
            result => result,
            done   => done
            );

    clk <= not clk and clk_en after 5 ns;

    process
        variable seed1, seed2 : positive := 1;
        variable rand_val     : real;

        variable passed, failed : integer;
    begin
        passed := 0;
        failed := 0;

        -- Reset the design.
        rst  <= '1';
        go   <= '0';
        data <= (others => '0');
        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;

        -- Clear reset.
        rst <= '0';
        wait until rising_edge(clk);

        for i in 0 to NUM_TESTS-1 loop
            -- Generate data input.
            uniform(seed1, seed2, rand_val);
            data <= std_logic_vector(to_unsigned(integer(floor(rand_val * real(2**WIDTH-1))), WIDTH));
            go   <= '1';
            wait until rising_edge(clk);
            go   <= '0';

            wait until rising_edge(clk) and done = '0';
            wait until rising_edge(clk) and done = '1';

            if (signed(result) = model(data)) then
                passed := passed + 1;
            else
                report "Test failed: result " & integer'image(to_integer(signed(result))) & " instead of " & integer'image(to_integer(model(data)));
                failed := failed + 1;
            end if;
        end loop;

        clk_en <= '0';
        report "Tests completed: " & integer'image(passed) & " passed, " & integer'image(failed) & " failed.";
        wait;
    end process;

    -- We also need to make sure that done is cleared within one cycle of go
    -- being asserted. With PSL or SystemVerilog, this is trivial:
    --
    -- assert property (@(posedge clk) disable iff (rst) go && done |=> !done);
    --
    -- However, in vanilla VHDL without PSL, we have to check this manually.
    -- I use the following process to know when I should check to see if
    -- done is '0'.

    process(clk, rst)
    begin
        if (rst = '1') then
            check_done_low <= '0';
        elsif (rising_edge(clk)) then
            check_done_low <= go and done;
        end if;        
    end process;

    -- I then use an assert to make sure done is low at the appropriate time.
    assert(not(rising_edge(clk) and check_done_low = '1' and done = '1')) report "Done not cleared after assertion of go";
    
end random_tb;
