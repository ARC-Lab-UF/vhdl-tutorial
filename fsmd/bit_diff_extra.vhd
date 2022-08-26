-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;

entity reg is
    generic(
        WIDTH : positive
        );
    port(
        clk    : in  std_logic;
        rst    : in  std_logic;
        en     : in  std_logic;
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end reg;

architecture default_arch of reg is
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            output <= (others => '0');
        elsif (rising_edge(clk)) then
            if (en = '1') then
                output <= input;
            end if;
        end if;
    end process;
end default_arch;


library ieee;
use ieee.std_logic_1164.all;

entity mux2x1 is
    generic (
        WIDTH : positive
        );    
    port(
        in0    : in  std_logic_vector(WIDTH-1 downto 0);
        in1    : in  std_logic_vector(WIDTH-1 downto 0);
        sel    : in  std_logic;
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end mux2x1;

architecture default_arch of mux2x1 is
begin
    output <= in0 when sel = '0' else in1;
end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity add is
    generic (
        WIDTH : positive
        );    
    port (
        in0, in1 : in  std_logic_vector(WIDTH-1 downto 0);
        sum      : out std_logic_vector(WIDTH-1 downto 0)
        );
end add;


architecture default_arch of add is
begin
    sum <= std_logic_vector(unsigned(in0) + unsigned(in1));
end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Avoiding name shift_right to avoid conflict with numeric_std function.
entity right_shift is
    generic (
        WIDTH        : positive;
        SHIFT_AMOUNT : natural
        );    
    port (
        input  : in  std_logic_vector(WIDTH-1 downto 0);
        output : out std_logic_vector(WIDTH-1 downto 0)
        );
end right_shift;

architecture default_arch of right_shift is
begin
    output <= std_logic_vector(shift_right(unsigned(input), SHIFT_AMOUNT));
end default_arch;


library ieee;
use ieee.std_logic_1164.all;

entity eq is
    generic (
        WIDTH : positive
        );    
    port (
        in0, in1 : in  std_logic_vector(WIDTH-1 downto 0);
        output   : out std_logic
        );
end eq;


architecture default_arch of eq is
begin
    output <= '1' when in0 = in1 else '0';
end default_arch;


-- Entity: datapath1
-- Description: This entity creates the illustrated datapath structurally.
--
-- See bit_diff.pdf for a graphical illustration of this datapath.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity datapath1 is
    generic (
        WIDTH : positive
        );    
    port (
        clk        : in  std_logic;
        rst        : in  std_logic;
        data       : in  std_logic_vector(WIDTH-1 downto 0);
        data_sel   : in  std_logic;
        data_en    : in  std_logic;
        diff_sel   : in  std_logic;
        diff_en    : in  std_logic;
        count_sel  : in  std_logic;
        count_en   : in  std_logic;
        result_en  : in  std_logic;
        count_done : out std_logic;
        result     : out std_logic_vector(integer(ceil(log2(real(WIDTH*2+1))))-1 downto 0)
        );
end datapath1;

architecture str1 of datapath1 is

    constant DIFF_WIDTH : integer := result'length;

    -- Another very strange VHDL 1993 restriction is that you can only use
    -- a non-locally static value (in this case, the generic WIDTH) if it is the
    -- only "choice" in the aggregrate.
    --
    -- This gives an error because WIDTH is not locally static.
    --constant C_1_WIDTH    : std_logic_vector(WIDTH-1 downto 0) := (WIDTH-1 downto 1 => '0', 0 => '1');
    -- This gives a warning in Modelsim since the range of others is not locally
    -- static.
    --constant C_1_WIDTH    : std_logic_vector(WIDTH-1 downto 0) := (0      => '1', others => '0');
    -- This version works because the non-locally static WIDTH is the only
    -- choice in the aggregate.
    --constant C_1_WIDTH    : std_logic_vector(WIDTH-1 downto 0) := (WIDTH-1 downto 0 => '0') & '1';

    -- You can always resort to this version. The casting is annoying but it
    -- will always work.
    constant C_1_WIDTH : std_logic_vector(WIDTH-1 downto 0) := std_logic_vector(to_unsigned(1, WIDTH));

    -- A simple way to set something to -1 is to set all the bits to '1'.
    constant C_NEG1_WIDTH : std_logic_vector(WIDTH-1 downto 0) := (others => '1');

    -- NOTE: The above constants are not used in the architecture, but could be
    -- used as an alternative to the to_unsigned functions and aggregrations
    -- in the code below.

    signal data_mux, data_r, data_shift            : std_logic_vector(WIDTH-1 downto 0);
    signal diff_r, add_in1_mux, diff_add, diff_mux : std_logic_vector(DIFF_WIDTH-1 downto 0);

    constant COUNT_WIDTH : integer := integer(ceil(log2(real(WIDTH))));    
    signal count_add, count_mux, count_r           : std_logic_vector(COUNT_WIDTH-1 downto 0);
    
begin
    -- Mux that defines provides input to the data register.
    -- The label uses a U_ prefix to avoid the naming conflict with the
    -- data_mux signal.
    U_DATA_MUX : entity work.mux2x1
        generic map (WIDTH => WIDTH)
        port map (
            in0    => data_shift,
            in1    => data,
            sel    => data_sel,
            output => data_mux
            );

    -- The data register.
    U_DATA_REG : entity work.reg
        generic map (WIDTH => WIDTH)
        port map (clk    => clk,
                  rst    => rst,
                  en     => data_en,
                  input  => data_mux,
                  output => data_r);

    -- Shifter for the data register.
    U_DATA_SHIFT : entity work.right_shift
        generic map (WIDTH        => WIDTH,
                     SHIFT_AMOUNT => 1)
        port map (input  => data_r,
                  output => data_shift);


    -- Selects a 1 or -1 input to the adder.
    U_ADD_MUX : entity work.mux2x1
        generic map (WIDTH => add_in1_mux'length)
        port map (
            -- Unfortunately, VHDL 1993 and 2001 are very restrictive about
            -- expressions that can appear in port map associations. You will
            -- often get the error "Actual expression is not globally static."
            -- Basically, this means that the expression
            -- in a port map has to be a single signal name or value that can
            -- be resolved during elaboration (an initial step of compilation).
            -- I can't find the actual definition from the standard, but it
            -- looks like there are some situations where you can call
            -- functions with at most one signal parameter. However, you cannot
            -- have arbitrary expressions here. In those cases, you need to
            -- declare another signal, compute the expression, and then use
            -- the new signal in the port mapping.
            --
            -- The code we use works because it is resolves to a constant
            -- at elaboration time. In some situations, I've seen code that
            -- should resolve to a constant have the same "non-locally static"
            -- error. In that case, you can just declare a constant in the
            -- architecture and use that constant in the port map.

            in0    => std_logic_vector(to_signed(-1, add_in1_mux'length)),
            in1    => std_logic_vector(to_signed(1, add_in1_mux'length)),
            sel    => data_r(0),
            output => add_in1_mux
            );

    -- Adds the current difference with the output of the add_in1_mux (1 or -1).
    U_ADD : entity work.add
        generic map (WIDTH => diff_r'length)
        port map (in0 => diff_r,
                  in1 => add_in1_mux,
                  sum => diff_add);

    -- Selects between 0 or the diff adder.
    U_DIFF_MUX : entity work.mux2x1
        generic map (WIDTH => diff_mux'length)
        port map (
            in0    => diff_add,
            in1    => (others => '0'),
            sel    => diff_sel,
            output => diff_mux
            );

    -- The diff register.
    U_DIFF_REG : entity work.reg
        generic map (WIDTH => diff_r'length)
        port map (clk    => clk,
                  rst    => rst,
                  en     => diff_en,
                  input  => diff_mux,
                  output => diff_r);

    -- The result register.
    U_RESULT_REG : entity work.reg
        generic map (WIDTH => result'length)
        port map (clk    => clk,
                  rst    => rst,
                  en     => result_en,
                  input  => diff_mux,
                  output => result);

    --------------------------------------------------------------------
    -- Counter logic.

    U_COUNT_MUX : entity work.mux2x1
        generic map (WIDTH => count_mux'length)
        port map (
            in0    => count_add,
            in1    => (others => '0'),
            sel    => count_sel,
            output => count_mux
            );


    U_COUNT_REG : entity work.reg
        generic map (WIDTH => count_r'length)
        port map (clk    => clk,
                  rst    => rst,
                  en     => count_en,
                  input  => count_mux,
                  output => count_r);


    U_COUNT_ADD : entity work.add
        generic map (WIDTH => count_r'length)
        port map (in0 => std_logic_vector(to_unsigned(1, count_r'length)),
                  in1 => count_r,
                  sum => count_add);

    U_EQ : entity work.eq
        generic map (WIDTH => count_r'length)
        port map (in0    => count_r,
                  in1    => std_logic_vector(to_unsigned(WIDTH-1, count_r'length)),
                  output => count_done);

end str1;


architecture str2 of datapath1 is

    signal data_mux, data_r, data_shift            : std_logic_vector(WIDTH-1 downto 0);
    signal diff_r, add_in1_mux, diff_add, diff_mux : std_logic_vector(result'range);
    signal count_add, count_mux, count_r           : std_logic_vector(WIDTH-1 downto 0);
    signal result_r                                : std_logic_vector(result'range);
    
begin
    -- Data mux and shift
    data_mux   <= data when data_sel = '1' else data_shift;
    data_shift <= std_logic_vector(shift_right(unsigned(data_r), 1));

    -- Add mux, diff adder, diff mux
    -- This can probably be simplfied by changing signal types to
    -- signed/unsigned.
    add_in1_mux <= std_logic_vector(to_unsigned(1, add_in1_mux'length)) when data_r(0) = '1' else std_logic_vector(to_signed(-1, add_in1_mux'length));
    diff_add    <= std_logic_vector(unsigned(diff_r) + unsigned(add_in1_mux));
    diff_mux    <= (others => '0')                         when diff_sel = '1'  else diff_add;

    count_mux  <= (others => '0') when count_sel = '1' else count_add;
    count_add  <= std_logic_vector(unsigned(count_r) + 1);
    count_done <= '1'             when count_r = std_logic_vector(to_unsigned(WIDTH-1, WIDTH));

    -- Not necessary, but compiles with my _r convention for registers.
    result <= result_r;

    -- Create the registers behaviorally.
    process(clk, rst)
    begin
        if (rst = '1') then
            data_r   <= (others => '0');
            diff_r   <= (others => '0');
            result_r <= (others => '0');
            count_r  <= (others => '0');
        elsif (rising_edge(clk)) then
            if (data_en = '1') then data_r     <= data_mux; end if;
            if (diff_en = '1') then diff_r     <= diff_mux; end if;
            if (result_en = '1') then result_r <= diff_mux; end if;
            if (count_en = '1') then count_r   <= count_mux; end if;
        end if;
    end process;
end str2;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity fsm1 is
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        go        : in  std_logic;
        count_done : in std_logic;
        done      : out std_logic;
        data_sel  : out std_logic;
        data_en   : out std_logic;
        diff_sel  : out std_logic;
        diff_en   : out std_logic;
        count_sel : out std_logic;
        count_en  : out std_logic;
        result_en : out std_logic
        );
end fsm1;

architecture default_arch of fsm1 is
    type state_t is (START, COMPUTE, RESTART);
    signal state_r, next_state : state_t;
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            state_r <= START;
        elsif (rising_edge(clk)) then
            state_r <= next_state;
        end if;
    end process;

    process(go, state_r, count_done)
    begin
        done <= '0';

        result_en <= '0';
        diff_en   <= '0';
        count_en  <= '0';
        data_en   <= '0';

        diff_sel  <= '0';
        count_sel <= '0';
        data_sel  <= '0';

        next_state <= state_r;

        case (state_r) is
            when START =>
                -- Replaces diff_r <= (others => '0')
                diff_en  <= '1';
                diff_sel <= '1';

                -- Replaces count_r <= (others => '0')
                count_en  <= '1';
                count_sel <= '1';

                -- Replaces data_r <= data
                data_en  <= '1';
                data_sel <= '1';

                if (go = '1') then
                    next_state <= COMPUTE;
                end if;

            when COMPUTE =>

                -- Selects are '0' by default and don't have to be respecified.

                -- Replaces if statement that defines diff_r.
                diff_en <= '1';

                -- Replaces data_r <= shift_right(data_r, 1)
                data_en <= '1';

                -- Replaces count_r <= count_r + 1;
                count_en <= '1';

                -- Replaces count_r = WIDTH-1
                if (count_done = '1') then
                    -- Enable the result register one cycle early to make sure
                    -- the register output aligns with the assertion of done.
                    result_en  <= '1';
                    next_state <= RESTART;
                end if;
                
            when RESTART =>
                done <= '1';

                -- Replaces diff_r <= (others => '0')
                diff_en  <= '1';
                diff_sel <= '1';

                -- Replaces count_r <= (others => '0')
                count_en  <= '1';
                count_sel <= '1';

                -- Replaces data_r <= data
                data_en  <= '1';
                data_sel <= '1';

                if (go = '1') then
                    next_state <= COMPUTE;
                end if;

            when others => null;
        end case;                
    end process;
    
end default_arch;
