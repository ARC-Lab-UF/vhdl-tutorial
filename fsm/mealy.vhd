-- Greg Stitt
-- University of Florida
--
-- This file illustrates how to create a Mealy FSM. Unlike a Moore FSM, where
-- outputs are solely associated with states, a Mealy FSM has outputs that are
-- a function of the state and the input, which means outputs get assigned on
-- transitions. See the mealy examples in fsm.pdf for an illustration of the
-- FSMs in each module.
--
-- This example omits a 1-process model, since I don't remember using it unless
-- you specifically want registered outputs.
--
-- MISC THOUGHTS: Most FSMs can be implemented as Moore or a Mealy. I find
-- Mealy FSMs to be more flexible because I can assign the same output different
-- values in the same state depending on the transition. In general, I use
-- a hybrid approach similar to the one shown below. Basically, I use Moore
-- unless I need something to change on a transition, in which I change that
-- specific location to act like a Mealy.

library ieee;
use ieee.std_logic_1164.all;

-- Module: mealy_example
-- Description: 2-process implementation of the Mealy FSM shown in
-- fsm.pdf

entity mealy_example is
    port (
        clk, rst, go, ack : in  std_logic;
        en, done          : out std_logic
        );
end mealy_example;

architecture two_process of mealy_example is

    type state_t is (START, COMPUTE, FINISH, RESTART);
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

    process (state_r, go, ack)
    begin
        case (state_r) is
            when START =>
                if (go = '1') then
                    done       <= '0';
                    en         <= '0';
                    next_state <= COMPUTE;
                else
                    done       <= '0';
                    en         <= '0';
                    next_state <= START;
                end if;

            when COMPUTE =>
                if (ack = '1') then
                    en         <= '0';
                    done       <= '1';
                    next_state <= FINISH;
                else
                    en         <= '1';
                    done       <= '0';
                    next_state <= COMPUTE;
                end if;

            when FINISH =>
                if (go = '1') then
                    done       <= '1';
                    en         <= '0';
                    next_state <= FINISH;
                else
                    done       <= '1';
                    en         <= '0';
                    next_state <= RESTART;
                end if;

            when RESTART =>
                if (go = '1') then
                    done       <= '0';
                    en         <= '0';
                    next_state <= COMPUTE;
                else
                    done       <= '1';
                    en         <= '0';
                    next_state <= RESTART;
                end if;
        end case;
    end process;
end two_process;


architecture hybrid of mealy_example is

    type state_t is (START, COMPUTE, FINISH, RESTART);
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

    process (state_r, go, ack)
    begin
        case (state_r) is
            when START =>

                -- If all ouptuts are the same for all transitions, the state
                -- can be simplfied into a Moore state to save some code. Note
                -- that this is just a coding simplification because if the
                -- FSM has any outputs that are specific to a transition, it
                -- is technically a Mealy FSM.
                done <= '0';
                en   <= '0';

                if (go = '1') then
                    next_state <= COMPUTE;
                else
                    next_state <= START;
                end if;

            when COMPUTE =>
                -- In this state, outputs differ on each transition, so we can't
                -- simplify the code.                
                if (ack = '1') then
                    en         <= '0';
                    done       <= '1';
                    next_state <= FINISH;
                else
                    en         <= '1';
                    done       <= '0';
                    next_state <= COMPUTE;
                end if;

            when FINISH =>
                -- This state can also be simplified.                
                done <= '1';
                en   <= '0';
                if (go = '1') then
                    next_state <= FINISH;
                else
                    next_state <= RESTART;
                end if;

            when RESTART =>
                -- In this state, en has the same value on all transitions, so
                -- it can be removed from the transitions. However, the done
                -- signal must be assigned for each transition.
                --
                -- Alteratively, done could be assigned a default value and then
                -- assigned when the default does not apply.    
                en <= '0';

                if (go = '1') then
                    done       <= '0';
                    next_state <= COMPUTE;
                else
                    done       <= '1';
                    next_state <= RESTART;
                end if;
        end case;
    end process;
end hybrid;

----------------------------------------------------------------------------
-- Top-level entity for evaluating the different architectures.

library ieee;
use ieee.std_logic_1164.all;

entity mealy is
    port (
        clk, rst, go, ack : in  std_logic;
        en, done          : out std_logic
        );
end mealy;

architecture default_arch of mealy is
begin

    U_MEALY : entity work.mealy_example(two_process)
        --U_MEALY : entity work.mealy_example(hybrid)
        port map (
            clk  => clk,
            rst  => rst,
            go   => go,
            ack  => ack,
            en   => en,
            done => done
            );



end default_arch;
