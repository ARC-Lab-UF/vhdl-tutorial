-- Greg Stitt
-- University of Florida
--
-- This file illustrates how to create a basic structural architecture by
-- combining three 2x1 muxes to create a 4x1 mux.
--
-- For any structural architecture, the most critical first step is to draw
-- out a schematic of the architecture in terms of entities that have already
-- been defined. The structural description is then simply a text representation
-- of that schematic.
--
-- See mux4x1.pdf for the corresponding schematic.


-- Module: mux2x1
-- Description: A basic 2x1 mux. We'll be using this to create a structural
-- 4x1 mux.

library ieee;
use ieee.std_logic_1164.all;

entity mux_2x1 is
    port(
        in0, in1, sel : in  std_logic;
        output        : out std_logic);
end mux_2x1;

architecture default_arch of mux_2x1 is
begin
    output <= in0 when sel = '0' else in1;
end default_arch;

-------------------------------------------------------------------------

-- Entity: mux4x1
-- Description: A structural implementation of a 4x1 mux using 3 separate 2x1
-- muxes. See mux4x1.pdf for a schematic of the architecture.

library ieee;
use ieee.std_logic_1164.all;

entity mux_4x1 is
    port(
        inputs : in  std_logic_vector(3 downto 0);
        sel    : in  std_logic_vector(1 downto 0);
        output : out std_logic
        );
end mux_4x1;

-- There are two different syntactic options for structural architectures.
-- The following architecture shows my recommended way, which requires the
-- least amount of code

architecture STR1 of mux_4x1 is

    -- Signals for internal connections between muxes.
    signal mux1_out, mux2_out : std_logic;

begin

    -- Instantiate the three muxes and connect them together as shown in
    -- the schematic.
    --
    -- To create an instance of an existing entity, you first give it a label
    -- (e.g. U_MUX1). You then specifiy "entity work.entity_name". The work
    -- keyword referring to the "working" library or directory, which for now
    -- you can think of as the directory where all your code gets compiled.
    -- So, the simulator or synthesis tool is looking for an entity in work
    -- called mux_2x1 to instantiate. If it doesn't exist, you will get an
    -- error.
    --
    -- Next, you need to specify the I/O connections for the instance, which is
    -- done with a port map. Port maps can specify connections positionally
    -- (i.e., based on the order that the I/O is declared), but I strongly
    -- recommend against doing this for several reasons. First, it is very
    -- error prone. Second, the port map doesn't explicitly show what
    -- connections are being made. You have to remember the order, which is
    -- not feasible for large ports.
    --
    -- The other option is named connections, which is shown below. The syntax
    -- corresponds to:
    --    instantiated_entity_signal_name => local_signal name
    -- which in this case would be:
    --    mux_2x1_signal_name => mux4x1_signal_name
    
    U_MUX1 : entity work.mux_2x1 port map (
        in0    => inputs(2),
        in1    => inputs(3),
        sel    => sel(0),
        output => mux1_out
        );

    U_MUX2 : entity work.mux_2x1 port map (
        in0    => inputs(0),
        in1    => inputs(1),
        sel    => sel(0),
        output => mux2_out
        );

    U_MUX3 : entity work.mux_2x1 port map (
        in0    => mux2_out,
        in1    => mux1_out,
        sel    => sel(1),
        output => output
        );

end STR1;


-- This archicture shows an alternative way instantiating other entities.

architecture STR2 of mux_4x1 is

    -- In the previous architecture, we are telling the compiler to look in
    -- a specific place for an entity with a specific name. Most of the time
    -- that works fine. However, there are situations where this won't work.
    -- For example, maybe your design is instantiating an entity from a
    -- different library, or maybe it is instantiating code from another
    -- language. In these cases, we might want to just tell the compiler
    -- the name and structure of what we'll be instantiating, and then let it
    -- search for the implementation later.
    --
    -- A "component" achieves this functionality, by providing the name and port
    -- of a circuit whose definition will be found by the compiler later.
    --
    -- Here we create a component for our 2:1 mux. It has the exact same name
    -- and port as the mux_2x1 entity. In fact, if you ever need a component
    -- copy and paste the entity, change entity to component, and get rid of the
    -- "is" keyword.
    --
    -- Note that this component is completely unnecessary for this example. In
    -- fact, it is usually unnecessary. I avoid it whenever possible because
    -- not only is it more code, if you happen to change the port structure of
    -- the entity, you have to change the component declaration everywhere you
    -- use them.
    --
    -- I generally only use components when instantiating vendor IP cores.
    
    component mux_2x1
        port(
            in0    : in  std_logic;
            in1    : in  std_logic;
            sel    : in  std_logic;
            output : out std_logic
            );
    end component;

    signal mux1_out, mux2_out : std_logic;

begin

    -- When using components, we simply omit the entity work. syntax because
    -- we are now instantiating components as opposed to entities.    
    U_MUX1 : mux_2x1 port map (
        in0    => inputs(2),
        in1    => inputs(3),
        sel    => sel(0),
        output => mux1_out
        );

    U_MUX2 : mux_2x1 port map (
        in0    => inputs(0),
        in1    => inputs(1),
        sel    => sel(0),
        output => mux2_out
        );

    U_MUX3 : mux_2x1 port map (
        in0    => mux2_out,
        in1    => mux1_out,
        sel    => sel(1),
        output => output
        );

end STR2;


-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-- Entity: mux4x1:
-- Description: Top-level entity for evaluating the different architectures.

entity mux4x1 is
    port(
        inputs : in  std_logic_vector(3 downto 0);
        sel    : in  std_logic_vector(1 downto 0);
        output : out std_logic
        );
end mux4x1;

architecture default_arch of mux4x1 is
begin
    -- INSTRUCTIONS: change the comments to evaluate each architecture.
    U_MUX : entity work.mux_4x1(STR1)
        --U_MUX : entity work.mux_4x1(STR2)
        port map (
            inputs => inputs,
            sel    => sel,
            output => output);
end default_arch;
