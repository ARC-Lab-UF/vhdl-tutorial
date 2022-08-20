-- Greg Stitt
-- University of Florida
--
-- This file demonstrates how to use a loop to generate a structural pattern
-- in a circuit. Specifically, it creates a ripple carry adder with a
-- parameterized width by instantiating full adders in a loop.
--
-- See ripple_carry_adder.pdf for an illustration of the schematic being
-- created. Remember that all strucutural architectures should start from a
-- schematic.

library ieee;
use ieee.std_logic_1164.all;

-- Entity: full_adder
-- Description: A basic behavioral implementation of a full adder.

entity full_adder is
    port (
        x, y, cin : in  std_logic;
        s, cout   : out std_logic
        );
end full_adder;

architecture default_arch of full_adder is
begin
    -- Specify the sum and carry out logic equations for a full adder.
    s    <= x xor y xor cin;
    cout <= (x and y) or (cin and (x xor y));
end default_arch;

-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-- Entity: ripple_carry_adder
-- Description: A structural ripple carry adder with a parameter for width,
-- built from the preceding full_adder entity. Demonstrates how to use a
-- for-generate loop.

entity ripple_carry_adder is
    generic (
        WIDTH : positive := 9
        );
    port (
        x, y : in  std_logic_vector(WIDTH-1 downto 0);
        cin  : in  std_logic;
        sum  : out std_logic_vector(WIDTH-1 downto 0);
        cout : out std_logic
        );
end ripple_carry_adder;

architecture default_arch of ripple_carry_adder is
    -- Create an internal signal to store the carries between all full adders.
    -- Note that this is WIDTH+1 bits to account for the overall carry out.
    signal carry : std_logic_vector(WIDTH downto 0);
    
begin
    -- Connect the first carry to the carry in.
    carry(0) <= cin;

    -- Instantiate WIDTH separate full adders using a for-generate loop, and
    -- connect them into a ripple-carry by connecting the carry out from one
    -- full adder into the carry in of the next.
    --
    -- Note that the for-generate is different from the for loop we saw in
    -- earlier examples. The for loop contains sequential statements and can
    -- only occur inside a process. The for-generate containts concurrent
    -- statements and entity instantiations. There is a slight syntax difference
    -- with loop being replaced by generate.

    RIPPLE_CARRY : for i in 0 to WIDTH-1 generate
        U_FA : entity work.full_adder
            port map (
                x    => x(i),
                y    => y(i),
                cin  => carry(i),
                s    => sum(i),
                cout => carry(i+1)
                );        
    end generate;

    -- Connect the last carry to the carry out.
    cout <= carry(WIDTH);
    
end default_arch;
