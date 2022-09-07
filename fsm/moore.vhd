-- Greg Stitt
-- University of Florida

-- The following example illustrates two models that I would recommend for
-- creating a finite state machine (FSM), which I refer to as the 
-- 1-process and 2-process models. The 1-process model is slightly 
-- easier, but has the disadvantage of having registers/flip-flops on all 
-- outputs, which adds one cycle of latency. The 2-process model requires a 
-- little more code, but is more flexible. When timing/area are not a concern, 
-- the 1-process model can save a little bit of time, but I would still 
-- recommend using the 2-process model. Once you get used to it, it requires 
-- only a minor amount of additional effort, and in my opinion is much easier 
-- to debug.

-- The following example implements the Moore FSM illustrated in fsm.pdf. A
-- Moore FSM has outputs that are solely a function of the current state.
-- Note that you should always draw the FSM first and then convert it to code.

library ieee;
use ieee.std_logic_1164.all;

entity moore is
    port (
        clk, rst, en : in  std_logic;
        output       : out std_logic_vector(3 downto 0)
        );
end moore;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- 1-process model examples
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Architecture: one_process_1
-- Description: This architecture implements the Moore FSM shown in fsm.pdf
-- using the 1-process model. It is important to note that the 1-process model
-- will delay all outputs by 1 cycle due to the extra register. This register
-- has its uses (e.g. preventing glitches), but in general is problematic for
-- any circuit requiring control signals within the same cycle.
--
-- COMMON MISTAKE: The 1-process model will often lead to accidentally created
-- registers. This mistake is a good reason to use my convention of creating
-- internal signals for each register, which makes it much harder to
-- accidentally make something a register.

architecture one_process_1 of moore is
    -- There are numerous ways to do state machines, but I recommend declaring
    -- your own type that defines each possible state. This makes the code more
    -- readable, and the state names will appear in the waveform simulation.
    --
    -- NAMING CONVENTION: I recommend the common convention of putting
    -- a _t suffix on custom type names.
    type state_t is (STATE0, STATE1, STATE2, STATE3);

    -- Create a signal to store the current state. Notice that its type is
    -- state_t, which we just defined. Also, the state is stored in a register,
    -- so we use an _r suffix on the signal name.
    signal state_r : state_t;
    
begin
    -- The 1-process FSM model treats the entire FSM like sequential logic in a
    -- single process/block.
    -- 
    -- In other words, all signals that are assigned values are implemented as
    -- registers. Therefore, we will follow the exact same synthesis guidelines
    -- as we did for sequential logic. 

    process(clk, rst)
    begin
        if (rst = '1') then
            -- Initialize outputs and state_r
            output  <= "0001";
            state_r <= STATE0;
            
        elsif(clk'event and clk = '1') then

            -- Specific the output logic and next-state logic for each state.
            -- This is as simple as translating each state from the FSM diagram.
            case state_r is
                when STATE0 =>
                    -- Whenever possible, use a template similar to something
                    -- below for each state. In this example, I define the
                    -- outputs and then define the next-state logic.
                    -- You can then simply copy and paste for each state, while
                    -- changing the appropriate values.

                    -- Output logic
                    output <= "0001";

                    -- Next-state logic
                    if (en = '1') then
                        state_r <= STATE1;
                    else
                        state_r <= STATE0;
                    end if;
                    
                when STATE1 =>

                    output <= "0010";

                    if (en = '1') then
                        state_r <= STATE2;
                    else
                        state_r <= STATE1;
                    end if;

                when STATE2 =>

                    output <= "0100";

                    if (en = '1') then
                        state_r <= STATE3;
                    else
                        state_r <= STATE2;
                    end if;

                when STATE3 =>

                    output <= "1000";

                    if (en = '1') then
                        state_r <= STATE0;
                    else
                        state_r <= STATE3;
                    end if;
                    
                when others => null;
            end case;
        end if;
    end process;
end one_process_1;


-- This architecture show a simplified version of the previous architecture.

architecture one_process_2 of moore is

    type state_t is (STATE0, STATE1, STATE2, STATE3);
    signal state_r : state_t;

    -- Here we make the register more explicit to avoid accidentally creating
    -- an extra register.
    signal output_r : std_logic_vector(output'range);
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            output_r <= "0001";
            state_r  <= STATE0;
            
        elsif(clk'event and clk = '1') then

            case state_r is
                when STATE0 =>
                    output_r <= "0001";

                    -- We don't need the else becuase state_r is already
                    -- a register and will preserve its current value.
                    if (en = '1') then
                        state_r <= STATE1;
                    end if;
                    
                when STATE1 =>

                    output_r <= "0010";

                    if (en = '1') then
                        state_r <= STATE2;
                    end if;

                when STATE2 =>

                    output_r <= "0100";

                    if (en = '1') then
                        state_r <= STATE3;
                    end if;

                when STATE3 =>

                    output_r <= "1000";

                    if (en = '1') then
                        state_r <= STATE0;
                    end if;
                    
                when others => null;
            end case;
        end if;
    end process;

    -- Assign the output register directly to the output.
    output <= output_r;
end one_process_2;


architecture one_process_3 of moore is

    type state_t is (STATE0, STATE1, STATE2, STATE3);
    signal state_r  : state_t;
    signal output_r : std_logic_vector(output'range);

    -- If we want to use a custom encoding for each state_t value, we can
    -- do the following (which uses a binary encoding).

    attribute enum_encoding            : string;
    attribute enum_encoding of state_t : type is "00 01 10 11";

    -- enum_encoding is a synthesis attribute defined by the IEEE standard
    -- to control the encoding of enumerated types. You only need this if you
    -- want the enumeration to use specific values. In many cases, it is best
    -- to leave these values to the synthesis tool to increase optimization
    -- potential.
    --
    -- WARNINGS: Use this with caution. Although a synthesis tool is supposed
    -- to use this encoding, there is a decent chance some tools will ignore it.
    -- If you have logic that is based on a specific encoding, it might not
    -- work in all tools. In addition, it will very likely result in differences
    -- between simulation and synthesis because the simulator will not use this
    -- encoding.
    --
    -- SUGGESTION: If you are going to use an enumerated type, either:
    -- 1) Don't specify the encoding, or
    -- 2) Specify the encoding, but don't have any logic that requires that
    --    encoding for correct functionality. e.g. don't compare with a
    --    specific encoding, and don't assign a specific encoding to a signal.
    --    Only use the enumerated type.     
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            output_r <= "0001";
            state_r  <= STATE0;
            
        elsif(clk'event and clk = '1') then

            case state_r is
                when STATE0 =>
                    output_r <= "0001";

                    if (en = '1') then
                        state_r <= STATE1;
                    end if;
                    
                when STATE1 =>

                    output_r <= "0010";

                    if (en = '1') then
                        state_r <= STATE2;
                    end if;

                when STATE2 =>

                    output_r <= "0100";

                    if (en = '1') then
                        state_r <= STATE3;
                    end if;

                when STATE3 =>

                    output_r <= "1000";

                    if (en = '1') then
                        state_r <= STATE0;
                    end if;
                    
                when others => null;
            end case;
        end if;
    end process;

    output <= output_r;
end one_process_3;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- 2-process model examples
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- The difference between the 1-process model and the 2-process model is that
-- the 2-process model implements an FSM as a state register combined with
-- combinational logic for next-state logic and output logic (which is how FSMs
-- are normally implemented). Because the 2-process model explictly uses
-- combinational logic, it avoids the output registers that are created by
-- the 1-process model. Avoiding these registers is critical when timing is
-- important, which will be illustrated in another example. Note that with the
-- provided testbench the output of the 1-process model is always 1 cycle
-- behind the 2-process model, which is caused by the register on the output.

architecture two_process_1 of moore is

    type state_t is (STATE0, STATE1, STATE2, STATE3);
    signal state_r, next_state : state_t;  -- the 2-process model
                                           -- uses state and next_state

begin
    -- Process to model the state register. Note that this is exactly the same
    -- code that was previously used to implement a register. This process
    -- assigns the state_r signal on a rising clock edge, which means that state
    -- is implemented as a register (which is what we want).
    process(clk, rst)
    begin
        if (rst = '1') then
            -- Set the initial state. You might be tempted to define an output
            -- here, like what was done in the 1-process model. However, this
            -- will cause a multiple driver error because we will also be
            -- assigning output in the next process. Remember: only one
            -- process can assign a signal. Since output is combinational, it
            -- should not be assigned in this process.
            state_r <= STATE0;
        elsif (rising_edge(clk)) then
            state_r <= next_state;
        end if;
    end process;

    -- Process to model the next-state logic and output logic.
    -- This process is defining combinational logic, so make sure to have all
    -- inputs to the logic in the sensitivity list. Note that this is not the
    -- same as the inputs to the entity. The combinational logic uses "state_r"
    -- as an input, but state_r is not an input to the entity.
    process(en, state_r)
    begin
        case state_r is
            when STATE0 =>
                -- I use a similar template as before, where I first define
                -- outputs and then define next-state logic. However, notice
                -- that I no longer define "state_r". Instead, I define
                -- "next_state", which in turn gets assigned to "state_r" on
                -- the next rising clock edge (which is exactly what we want).
                -- In other words, state_r is the output of the register
                -- and next_state is the input to the register.
                
                output <= "0001";

                -- Since this is combinational logic, we need to avoid latches
                -- by defining next_state (an output of the logic, but not the
                -- entity) on all paths. The next architecture shows a better
                -- way of doing this.
                if (en = '1') then
                    next_state <= STATE1;
                else
                    next_state <= STATE0;
                end if;

            when STATE1 =>

                output <= "0010";

                if (en = '1') then
                    next_state <= STATE2;
                else
                    next_state <= STATE1;
                end if;

            when STATE2 =>

                output <= "0100";

                if (en = '1') then
                    next_state <= STATE3;
                else
                    next_state <= STATE2;
                end if;

            when STATE3 =>

                output <= "1000";

                if (en = '1') then
                    next_state <= STATE0;
                else
                    next_state <= STATE3;
                end if;

            when others => null;
        end case;
    end process;
end two_process_1;


-- This is a simplified version of the previous architecture.

architecture two_process_2 of moore is

    type state_t is (STATE0, STATE1, STATE2, STATE3);
    signal state_r, next_state : state_t;
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            state_r <= STATE0;
        elsif(clk'event and clk = '1') then
            state_r <= next_state;
        end if;
    end process;

    process(en, state_r)
    begin
        -- In this example, we use a default value for next_state to simplify
        -- the next-state logic. The following line of code assumes that
        -- next_state is equal to state_r unless we specify otherwise later in
        -- the process.

        next_state <= state_r;

        case state_r is
            when STATE0 =>

                output <= "0001";

                -- Because we use a default next_state, we don't need the else
                -- from the previous architecture.
                if (en = '1') then
                    next_state <= STATE1;
                end if;

            when STATE1 =>

                output <= "0010";

                if (en = '1') then
                    next_state <= STATE2;
                end if;

            when STATE2 =>

                output <= "0100";

                if (en = '1') then
                    next_state <= STATE3;
                end if;

            when STATE3 =>

                output <= "1000";

                if (en = '1') then
                    next_state <= STATE0;
                end if;

            when others => null;
        end case;
    end process;
end two_process_2;



-- This is the same code as the previous architecture, but with a different
-- naming convention.

architecture two_process_3 of moore is

    type state_t is (STATE0, STATE1, STATE2, STATE3);

    -- I often see _q used instead of _r, since q is often used to represent
    -- the output of a register or flip-flop. Similarly, _d can be used instead
    -- of "next", since d represents the input to a register.
    --
    -- For a pure FSM, I prefer my _r/next convention. Some people might not
    -- like that I use a next_ prefix instead of a _next suffix. The reason I
    -- prefer this is that it reads like I would speak it. The FSM has
    -- "next-state" logic, so I used next_state.
    --
    -- I think the _q/_d convention makes more sense for FSMDs, which are
    -- explained later. I still tend to prefer the _r/next convention, because
    -- my combinational logic should only ever assign outputs or next_ signals.
    -- "Next_" is very hard to miss, whereas there is a much smaller
    -- difference between _q/_d.
    --
    -- In any case, either convention is fine. Just be consistent within the
    -- same entity.
    signal state_q, state_d : state_t;
    
begin
    process(clk, rst)
    begin
        if (rst = '1') then
            state_q <= STATE0;
        elsif(clk'event and clk = '1') then
            state_q <= state_d;
        end if;
    end process;

    process(en, state_q)
    begin
        state_d <= state_q;

        case state_q is
            when STATE0 =>

                output <= "0001";

                if (en = '1') then
                    state_d <= STATE1;
                end if;

            when STATE1 =>

                output <= "0010";

                if (en = '1') then
                    state_d <= STATE2;
                end if;

            when STATE2 =>

                output <= "0100";

                if (en = '1') then
                    state_d <= STATE3;
                end if;

            when STATE3 =>

                output <= "1000";

                if (en = '1') then
                    state_d <= STATE0;
                end if;

            when others => null;
        end case;
    end process;
end two_process_3;


-------------------------------------------------------------------------------
-- Default architecture for evaluating all the architectures.

architecture default_arch of moore is
begin
    -- INSTRUCTIONS: Uncomment the version you would like to test.
    -- Note that the 1-process version cause the testbench to fail because
    -- of the 1-cycle delay on the output.
    
    --U_MOORE : entity work.moore(one_process_1)
    --U_MOORE : entity work.moore(one_process_2)
    --U_MOORE : entity work.moore(one_process_3)
    U_MOORE : entity work.moore(two_process_1)
    --U_MOORE : entity work.moore(two_process_2)
    --U_MOORE : entity work.moore(two_process_3)

        port map (
            clk => clk,
            rst => rst,
            en => en,
            output => output
            );
end default_arch;
