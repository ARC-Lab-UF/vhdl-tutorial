-- Greg Stitt
-- StittHub (www.stitt-hub.com)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The following module provides standard single dual-port RAM behavior that
-- is supported by most FPGAs. Some FPGAs use block RAMs that don't directly 
-- support an read enable, but they can implement it with extra logic. If you
-- don't need the read enable, simply set it to 1 when you instantiate this
-- module and this should synthesize directly onto RAM resources in most FPGAs.
--
-- One important thing to note is that this provides a "read-first" read-during-write
-- behavior. In other words, when the read port and and write port use the same
-- address, the read returns the old data, not the data currently being written.

entity ram_sdp_basic is
    generic (
        DATA_WIDTH : positive := 16;
        ADDR_WIDTH : positive := 10
        );
    port (
        clk     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp_basic;

architecture default_arch of ram_sdp_basic is
    type ram_t is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal ram : ram_t;

begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (wr_en = '1') then
                ram(to_integer(unsigned(wr_addr))) <= wr_data;
            end if;

            if (rd_en = '1') then
                rd_data <= ram(to_integer(unsigned(rd_addr)));
            end if;
        end if;
    end process;

end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The following module demonstrates my suggested manual logic for implementing
-- write-first behavior. There are other templates for inferring write-first, but
-- I don't recommend them. See my SystemVerilog tutorial for a more detailed
-- explanation.

entity ram_sdp_write_first is
    generic (
        DATA_WIDTH : positive := 16;
        ADDR_WIDTH : positive := 10
        );
    port (
        clk     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp_write_first;

architecture default_arch of ram_sdp_write_first is
    type ram_t is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal ram : ram_t;

    signal rd_data_ram    : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal bypass_data_r  : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal bypass_valid_r : std_logic;

begin
    -- Save write data in a register in case of a read-during-write. This 
    -- register "bypasses" the read from RAM to provide the new write data. 
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (rd_en = '1') then
                bypass_valid_r <= '0';
                if (wr_en = '1') then
                    bypass_data_r <= wr_data;
                    if (rd_addr = wr_addr) then
                        bypass_valid_r <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process;

    process (clk)
    begin
        if (rising_edge(clk)) then
            if (wr_en = '1') then
                ram(to_integer(unsigned(wr_addr))) <= wr_data;
            end if;

            if (rd_en = '1') then
                rd_data_ram <= ram(to_integer(unsigned(rd_addr)));
            end if;
        end if;
    end process;

    -- Mux to select from the memory or the bypass register in the event of a
    -- read-during-write. 
    rd_data <= bypass_data_r when bypass_valid_r = '1' else rd_data_ram;

end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- This module adds a register read data output, which is very commonly 
-- provided by RAM resources across most FPGAs. It is also generally a very good
-- idea to register the read data to improve clock frequencies, as the time to
-- read from memory creates a long logic delay.
--
-- Note that not all FPGAs provide RAMs that support a read enable on the 
-- registered output (or even on the read port at all). This template still 
-- works on those FPGAs, but the register simply won't be packed into the RAM
-- primitive. This is easy to check for your FPGAs. Synthesize the design and 
-- look at the resource counts, or the schematic. If the register is packed into
-- the RAM resource, there will be no registers used in your design. If it isn't
-- packed into the RAM resource, you will see registers in the resource counts,
-- and in the schematic.
--
-- Like the basic template, if you don't need the read enable, just connect it
-- to 1'b1 when you instantiate it, and the output register should be packed
-- on most FPGAs.

entity ram_sdp_output_reg is
    generic (
        DATA_WIDTH : positive := 16;
        ADDR_WIDTH : positive := 10
        );
    port (
        clk     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp_output_reg;

architecture default_arch of ram_sdp_output_reg is
    type ram_t is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal ram         : ram_t;
    signal rd_data_ram : std_logic_vector(DATA_WIDTH-1 downto 0);

begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (wr_en = '1') then
                ram(to_integer(unsigned(wr_addr))) <= wr_data;
            end if;

            if (rd_en = '1') then
                rd_data_ram <= ram(to_integer(unsigned(rd_addr)));
            end if;
        end if;
    end process;

    process (clk)
    begin
        if (rising_edge(clk)) then
            if (rd_en = '1') then
                rd_data <= rd_data_ram;
            end if;
        end if;
    end process;
end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- In this module, we combine all the above functionality into a single, 
-- generalized module with parameters that control each option.
--
-- We also add a STYLE string parameter that controls the type of RAM that
-- is used. See the documentation for ramstyle or ram_style your specific 
-- synthesis tools and FPGA to determine the valid values.

entity ram_sdp_general is
    generic (
        DATA_WIDTH  : positive := 16;
        ADDR_WIDTH  : positive := 10;
        REG_RD_DATA : boolean  := false;
        WRITE_FIRST : boolean  := false;
        STYLE       : string   := ""
        );
    port (
        clk     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp_general;

architecture default_arch of ram_sdp_general is
    type ram_t is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal ram                : ram_t;
    -- Tell Quartus what type of RAM to use
    attribute ramstyle        : string;
    attribute ramstyle of ram : signal is STYLE;

    -- Tell Vivado what type of RAM to use
    attribute ram_style        : string;
    attribute ram_style of ram : signal is STYLE;

    signal rd_data_ram : std_logic_vector(DATA_WIDTH-1 downto 0);
begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (wr_en = '1') then
                ram(to_integer(unsigned(wr_addr))) <= wr_data;
            end if;

            if (rd_en = '1') then
                rd_data_ram <= ram(to_integer(unsigned(rd_addr)));
            end if;
        end if;
    end process;

    l_write_first : if (WRITE_FIRST) generate
        signal bypass_data_r  : std_logic_vector(DATA_WIDTH-1 downto 0);
        signal bypass_valid_r : std_logic := '0';
    begin
        process (clk)
        begin
            if (rising_edge(clk)) then
                if (rd_en = '1') then
                    bypass_valid_r <= '0';
                    if (wr_en = '1') then
                        bypass_data_r <= wr_data;
                        if (rd_addr = wr_addr) then
                            bypass_valid_r <= '1';
                        end if;
                    end if;
                end if;
            end if;
        end process;

        l_reg_rd_data : if (REG_RD_DATA) generate
            process (clk)
            begin
                if (rising_edge(clk)) then
                    if (rd_en = '1') then
                        if (bypass_valid_r = '1') then
                            rd_data <= bypass_data_r;
                        else
                            rd_data <= rd_data_ram;
                        end if;
                    end if;
                end if;
            end process;
        end generate;

        l_no_reg_rd_data : if (not REG_RD_DATA) generate
            rd_data <= bypass_data_r when bypass_valid_r = '1' else rd_data_ram;
        end generate;
    end generate;

    l_read_first : if (not WRITE_FIRST) generate
    begin
        l_reg_rd_data : if (REG_RD_DATA) generate
            process (clk)
            begin
                if (rising_edge(clk)) then
                    if (rd_en = '1') then
                        rd_data <= rd_data_ram;
                    end if;
                end if;
            end process;
        end generate;

        l_no_reg_rd_data : if (not REG_RD_DATA) generate
            rd_data <= rd_data_ram;
        end generate;
    end generate;
end default_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Change the ARCH string in the following module to synthesize or simulate
-- different versions of the RAM.

entity ram_sdp is
    generic (
        DATA_WIDTH  : positive := 16;
        ADDR_WIDTH  : positive := 10;
        REG_RD_DATA : boolean  := false;
        WRITE_FIRST : boolean  := false;
        STYLE       : string   := "block";
        ARCH        : string   := "general"
        );
    port (
        clk     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp;

architecture default_arch of ram_sdp is
begin
    l_basic : if (ARCH = "basic") generate
    begin
        RAM : entity work.ram_sdp_basic
            generic map (
                DATA_WIDTH => DATA_WIDTH,
                ADDR_WIDTH => ADDR_WIDTH
                )
            port map (
                clk     => clk,
                rd_en   => rd_en,
                rd_addr => rd_addr,
                rd_data => rd_data,
                wr_en   => wr_en,
                wr_addr => wr_addr,
                wr_data => wr_data
                );
    end generate;

    l_write_first : if (ARCH = "write_first") generate
    begin
        RAM : entity work.ram_sdp_write_first
            generic map (
                DATA_WIDTH => DATA_WIDTH,
                ADDR_WIDTH => ADDR_WIDTH
                )
            port map (
                clk     => clk,
                rd_en   => rd_en,
                rd_addr => rd_addr,
                rd_data => rd_data,
                wr_en   => wr_en,
                wr_addr => wr_addr,
                wr_data => wr_data
                );
    end generate;

    l_output_reg : if (ARCH = "output_reg") generate
    begin
        RAM : entity work.ram_sdp_output_reg
            generic map (
                DATA_WIDTH => DATA_WIDTH,
                ADDR_WIDTH => ADDR_WIDTH
                )
            port map (
                clk     => clk,
                rd_en   => rd_en,
                rd_addr => rd_addr,
                rd_data => rd_data,
                wr_en   => wr_en,
                wr_addr => wr_addr,
                wr_data => wr_data
                );
    end generate;

    l_general : if (ARCH = "general") generate
    begin
        RAM : entity work.ram_sdp_general
            generic map (
                DATA_WIDTH  => DATA_WIDTH,
                ADDR_WIDTH  => ADDR_WIDTH,
                REG_RD_DATA => REG_RD_DATA,
                WRITE_FIRST => WRITE_FIRST,
                STYLE       => STYLE
                )
            port map (
                clk     => clk,
                rd_en   => rd_en,
                rd_addr => rd_addr,
                rd_data => rd_data,
                wr_en   => wr_en,
                wr_addr => wr_addr,
                wr_data => wr_data
                );
    end generate;
end default_arch;
