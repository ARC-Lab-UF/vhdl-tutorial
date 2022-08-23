-- Greg Stitt
-- University of Florida

-- This file implements a new package for the counter that includes a function
-- for more concisely computing the number of bits.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package counter_pkg is

    -- Function: clog2()
    -- Description: Takes the ceil(log2) of a positive integer.
    -- Useful for avoiding all the casting that takes place in math_real.
    function clog2(input : positive) return positive;

end package;

package body counter_pkg is

    function clog2(input : positive) return positive is
    begin
        return positive(ceil(log2(real(input))));
    end function;

end package body;
