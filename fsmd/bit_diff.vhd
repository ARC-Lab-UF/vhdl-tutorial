-- Greg Stitt
-- University of Florida
--
-- This file illustrates how to create a controller and datapath to implement
-- a higher-level algorithm. See bit_diff.pdf for an illustration of the
-- different implementations.
--
-- The algorithm being implemented is a bit-difference calculator. Given a
-- parameter for a specified WIDTH, the modules calculate the difference
-- between the number of 1s and 0s. E.g., if there are 3 more 1s than 0s, the
-- out is 3. If there are 3 more 0s than 1s, the output is -3.
--
-- Note: There are dozens of ways of implementing the bit difference
-- calculator. The following examples are not necessarily the most efficient,
-- and are simply used to introduce the FSMD and FSM+D models. 
--
-- The examples illustrate two different categories of controller+datapath
-- strategies: FSMDs and FSM+Ds. An FSMD specifies the controller and datapath
-- in a combined behavioral description. An FSM+D specifies an explicit datapath
-- and a separate controller, which are then connected together.
--
-- FSMDs are demonstrated in two ways: a 1-process and 2-process model. Similar
-- to the FSMs, the 1-process model registers everything and the 2-process
-- model provides the flexibility to decide what is registered and what isn't.

--=============================================================================
-- Parameter Descriptions
--
-- WIDTH : An integer representing the bits of the input data (should be > 0) 
--=============================================================================

--==============================================================================
-- Interface Description (all control inputs are active high)
--- INPUTS ---
-- clk   : Clock
-- rst   : Asynchronous reset
-- go    : Asserting starts the calculator for the specific data input. Has no
--         impact when the module is currently active (!done).
-- data  : The input to be used to calculate the bit difference

--- OUTPUTS ---
--result : The calculated result. Is valid when done is asserted.
-- done : Asserted when the result is valid. Remains asserted indefinitely
--        until go is asserted again, and then is cleared on the next cycle.
-- ============================================================================ 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity bit_diff_example is
    generic (
        WIDTH : positive
        );
    port (
        clk    : in  std_logic;
        rst    : in  std_logic;
        go     : in  std_logic;
        data   : in  std_logic_vector(WIDTH-1 downto 0);
        result : out std_logic_vector(WIDTH-1 downto 0);
        done   : out std_logic
        );          
end bit_diff_example;


----------------------------------------------------------------------------
-- FSMD implementations

-- Architecture: fsmd_1p
-- Description: A 1-process FSMD implementation of the calculator.
--
-- See the FSMD illustration in bit_diff.pdf for a graphical representation
-- of this module.

architecture fsmd_1p of bit_diff_example is
    -- Like the FSM, we define the states using a custom type.
    type state_t is (START, COMPUTE, RESTART);

    -- We are only using one process, so we just need the state_r signal.
    signal state_r : state_t;

    -- Create signals for the internal registers.
    signal data_r   : std_logic_vector(data'range);
    signal result_r : std_logic_vector(result'range);
    signal count_r  : unsigned(integer(ceil(log2(real(WIDTH))))-1 downto 0);
    signal diff_r   : signed(result'range);
    signal done_r   : std_logic;
    
begin
    -- These concurrent assignments aren't necessary, but they preserve the 
    -- naming convention of having all registers use a _r suffix without 
    -- requiring the outputs to have the suffix. I prefer to not name the
    -- outputs based on the internal representation for several reasons. First,
    -- there may be parameters for an entitty that change whether or not the
    -- output is registered, which could make the suffix misleading. Second,
    -- the user of the entity usually doesn't need to know if an output is
    -- registered. They might need to know the timing, but that can be specified
    -- in documentation, which is more meaningful than just knowing the 
    -- existence of a register. Finally, retiming might move all the registers
    -- anyway, in which case there might not be a register on the output.
    
    result <= result_r;
    done   <= done_r;

    process(clk, rst)
    begin
        if (rst = '1') then
            result_r <= (others => '0');
            done_r   <= '0';
            diff_r   <= (others => '0');
            count_r  <= (others => '0');
            data_r   <= (others => '0');
            state_r  <= START;
            
        elsif (rising_edge(clk)) then

            done_r <= '0';

            case (state_r) is
                when START =>
                    done_r   <= '0';
                    result_r <= (others => '0');
                    diff_r   <= (others => '0');
                    count_r  <= (others => '0');
                    data_r   <= data;

                    if (go = '1') then
                        state_r <= COMPUTE;
                    end if;

                when COMPUTE =>
                    -- Add one to the difference if asserted, else subtract one.
                    if (data_r(0) = '1') then
                        diff_r <= diff_r + 1;
                    else
                        diff_r <= diff_r - 1;
                    end if;

                    -- Shift out the current lowest bit.
                    data_r <= std_logic_vector(shift_right(unsigned(data_r), 1));

                    -- Update the count.
                    count_r <= count_r + 1;

                    -- We are done after checking WIDTH bits. The -1 is used
                    -- because the count_r assignment doesn't get updated until
                    -- the end of the time step (i.e. is non-blocking), which
                    -- means that count_r hasn't been updated with the next
                    -- value yet.
                    -- 
                    -- When subtracting from a signal, this would create an 
                    -- extra subtractor, which we definitely want to avoid.
                    -- However, in this case, WIDTH is a parameter, which is
                    -- treated as a constant. Synthesis will do constant
                    -- propagation and replace WIDTH-1 with a constant, so this
                    -- will just be a comparator.

                    if (count_r = WIDTH-1) then
                        state_r <= RESTART;
                    end if;

                    
                when RESTART =>
                    -- This state could easily be combined with START, but was
                    -- done this way on purpose to match the FSM+D version,
                    -- where done is not registered.

                    -- Assign outputs.
                    result_r <= std_logic_vector(diff_r);
                    done_r   <= '1';

                    -- Reset internal state.
                    count_r <= (others => '0');
                    data_r  <= data;

                    if (go = '1') then
                        -- We need to clear diff_r here in case the FSMD stays
                        -- in the restart state for multiple cycles, in which
                        -- case result_r would only be valid for 1 cycle.
                        diff_r <= (others => '0');

                        -- If we don't clear done here, done will remain
                        -- asserted for once cycle after go. We defined the
                        -- corrected behavior as done being cleared on the
                        -- cycle following the assertion of go.
                        done_r  <= '0';
                        state_r <= COMPUTE;
                    end if;

                when others => null;
            end case;
        end if;
    end process;
    
end fsmd_1p;


-- Architecture: fsmd_1p_2.
-- Description: This is an alternative to the previous 1-process FSMD that 
-- shrinks the FSM down to two states. Which one is better likely depends on
-- the targeted FPGA and the synthesis tool being used. In general, don't worry
-- about minor optimizations such as this until the module becomes a bottleneck
-- in a larger application. It is counter-intuitive, but pre-optimizing a 
-- module can often decrease quality of a larger application due to increased
-- routing complexity. For anything that isn't a bottleneck, I generally prefer
-- an implementation that provides a decent balance of brevity and readability.
-- When I do optimize, I usually have a specific tradeoff in mind. e.g. Minimize
-- resources, maximize clock frequency, etc. Often these goals are opposing,
-- which is another reason not to pre-optimize until you know the appropriate
-- tradeoff your are looking for.

architecture fsmd_1p_2 of bit_diff_example is
    -- We only have two states in this architecture.
    type state_t is (START, COMPUTE);
    signal state_r : state_t;

    signal data_r   : std_logic_vector(data'range);
    signal result_r : std_logic_vector(result'range);
    signal count_r  : unsigned(integer(ceil(log2(real(WIDTH))))-1 downto 0);
    signal diff_r   : signed(result'range);
    signal done_r   : std_logic;
    
begin
    result <= result_r;
    done   <= done_r;

    process(clk, rst)
        variable next_diff : signed(diff_r'range);
    begin
        if (rst = '1') then
            result_r <= (others => '0');
            done_r   <= '0';
            diff_r   <= (others => '0');
            count_r  <= (others => '0');
            data_r   <= (others => '0');
            state_r  <= START;
            
        elsif (rising_edge(clk)) then
            case (state_r) is
                when START =>
                    -- In this version, we no longer set done_r to 0 in this
                    -- state. We omit this code because reset will initialize
                    -- the done register. Also, we want to eliminate the
                    -- restart state, which requires the START state to provide
                    -- the same done functionality. To accomplish that goal,
                    -- the new START state simply preserves the value of done_r.
                    
                    diff_r  <= (others => '0');
                    count_r <= (others => '0');
                    data_r  <= data;

                    if (go = '1') then
                        -- In this version, we have to clear done when the
                        -- circuit is started because the START state just
                        -- preserves the previous value.
                        done_r  <= '0';
                        state_r <= COMPUTE;
                    end if;

                when COMPUTE =>
                    -- We use a variable here because we need to potentially
                    -- assign the new diff value to result_r in this cycle.
                    if (data_r(0) = '1') then
                        next_diff := diff_r + 1;
                    else
                        next_diff := diff_r - 1;
                    end if;

                    diff_r <= next_diff;

                    -- Shift out the current lowest bit.
                    data_r <= std_logic_vector(shift_right(unsigned(data_r), 1));

                    -- Update the count.
                    count_r <= count_r + 1;

                    if (count_r = WIDTH-1) then
                        -- By assigning these here, we can eliminate the
                        -- previous RESTART state.
                        done_r   <= '1';
                        result_r <= std_logic_vector(next_diff);
                        state_r  <= START;
                    end if;

                when others => null;
            end case;
        end if;
    end process;
    
end fsmd_1p_2;


-- Architecture: fsmd_2p
-- Description: Implements a 2-process version of the fsmd_1p architecture.

architecture fsmd_2p of bit_diff_example is

    -- Like the 2-process FSM, we have a state_r and next_state signal.
    type state_t is (START, COMPUTE, RESTART);
    signal state_r, next_state : state_t;

    -- For a 2-process FSMD, every register needs a signal for the output of
    -- the register, which is the current value represented by the _r suffix,
    -- and a signal for the input to the register (i.e., the value for the
    -- next cycle), which is determined by combinational logic.
    --
    -- You don't always have to do this for all registers, which we will show
    -- in later examples.

    signal data_r, next_data     : std_logic_vector(data'range);
    signal result_r, next_result : std_logic_vector(result'range);
    signal count_r, next_count   : unsigned(integer(ceil(log2(real(WIDTH))))-1 downto 0);
    signal diff_r, next_diff     : signed(result'range);
    signal done_r, next_done     : std_logic;
    
begin
    result <= result_r;
    done   <= done_r;

    -- The first process simply implements all the registers.
    process(clk, rst)
    begin
        if (rst = '1') then
            result_r <= (others => '0');
            done_r   <= '0';
            diff_r   <= (others => '0');
            count_r  <= (others => '0');
            data_r   <= (others => '0');
            state_r  <= START;
            
        elsif (rising_edge(clk)) then
            result_r <= next_result;
            done_r   <= next_done;
            diff_r   <= next_diff;
            count_r  <= next_count;
            data_r   <= next_data;
            state_r  <= next_state;
        end if;
    end process;

    -- The second process implements any combinational logic, which includes
    -- the inputs to all the registers, and any other combinational logic. For
    -- example, in this architecture the done output is not registered like in
    -- the 1-process model. Although the 2-process model seems like overkill for
    -- this example, the advantage is that you can control exactly what is
    -- registered. For complex designs, registering everything is usually not
    -- ideal, which makes the 2-process model useful. Also, there are ways to
    -- make it more concise in later examples.
    --
    -- VHDL 2008's process(all) would be helpful here. To make sure you aren't
    -- missing any signals from the sensitivity list, synthesize once before
    -- simulating. Synthesis usually gives warnings about signals missing from
    -- the sensitivity list, which can save you a lot of time debugging.
    process(go, result_r, done_r, diff_r, data_r, count_r, state_r)
        variable next_diff_v : signed(diff_r'range);
    begin
        -- Since this is combinational logic, we should never be assigning a
        -- _r version of the signals. The left hand side should either be a
        -- next_ signal, or other signals that correspond to combinational
        -- logic.
        --
        -- Here we assign default values to all the register inputs to make sure
        -- we don't have latches. For a register, a good default value is
        -- usually the current value because then we only have to assign the
        -- signal later if the register is going to change.
        next_result <= result_r;
        next_done   <= done_r;
        next_diff   <= diff_r;
        next_data   <= data_r;
        next_count  <= count_r;
        next_state  <= state_r;

        case (state_r) is
            when START =>

                next_done   <= '0';
                next_result <= (others => '0');
                next_diff   <= (others => '0');
                next_data   <= data;
                next_count  <= (others => '0');

                -- Without the default assignment at the beginning of the
                -- process, this would result in a latch in the 2-process FSMD.
                if (go = '1') then
                    next_state <= COMPUTE;
                end if;

            when COMPUTE =>
                if (data_r(0) = '1') then
                    next_diff_v := diff_r + 1;
                else
                    next_diff_v := diff_r - 1;
                end if;

                next_diff  <= next_diff_v;
                next_data  <= std_logic_vector(shift_right(unsigned(data_r), 1));
                next_count <= count_r + 1;


                -- Here, we could compare with next_count also and get rid of
                -- the -1. However, that would be non-ideal for two reasons.
                -- First, the addition for the count becomes an input to the
                -- comparator without a register in between, which could
                -- increase the length of the critical path and slow down the
                -- clock. Second, the count signal would need an extra bit for
                -- the new condition to ever be true, which would increase the
                -- size of the adder, the comparator, and the register. 
                if (count_r = WIDTH-1) then
                    next_done   <= '1';
                    next_result <= std_logic_vector(next_diff_v);
                    next_state  <= RESTART;
                end if;

            when RESTART =>
                -- The restart state here doesn't really do much
                -- different than the start state, so we could easily
                -- combine them like before.
                next_diff  <= (others => '0');
                next_count <= (others => '0');
                next_data  <= data;

                if (go = '1') then
                    next_done  <= '0';
                    next_state <= COMPUTE;
                end if;
                
            when others => null;
        end case;
    end process;
end fsmd_2p;



library ieee;
use ieee.std_logic_1164.all;

entity bit_diff is
    generic (
        WIDTH : positive
        );
    port (
        clk    : in  std_logic;
        rst    : in  std_logic;
        go     : in  std_logic;
        data   : in  std_logic_vector(WIDTH-1 downto 0);
        result : out std_logic_vector(WIDTH-1 downto 0);
        done   : out std_logic
        );          
end bit_diff;

architecture default_arch of bit_diff is
begin
    --U_BIT_DIFF : entity work.bit_diff_example(fsmd_1p)
    --U_BIT_DIFF : entity work.bit_diff_example(fsmd_1p_2)
    U_BIT_DIFF : entity work.bit_diff_example(fsmd_2p)
        generic map (WIDTH => WIDTH)
        port map (
            clk    => clk,
            rst    => rst,
            go     => go,
            data   => data,
            result => result,
            done   => done
            );

end default_arch;
