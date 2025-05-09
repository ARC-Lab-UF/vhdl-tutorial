-- Greg Stitt
-- StittHub (www.stitt-hub.com)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- In this module, we combine all the above functionality into a single, 
-- generalized module with parameters that control each option.
--
-- We also add a STYLE string parameter that controls the type of RAM that
-- is used. See the documentation for ramstyle or ram_style your specific 
-- synthesis tools and FPGA to determine the valid values.

entity ram_sdp_with_reset is
    generic (
        DATA_WIDTH  : positive := 16;
        ADDR_WIDTH  : positive := 10;
        REG_RD_DATA : boolean  := false;
        WRITE_FIRST : boolean  := false;
        STYLE       : string   := ""
        );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        rd_en   : in  std_logic;
        rd_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        rd_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
        wr_en   : in  std_logic;
        wr_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data : in  std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_sdp_with_reset;

architecture default_arch of ram_sdp_with_reset is
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
    -- IMPORTANT: Make sure to not reset anything here or it likely won't be
    -- inferred as a RAM.
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
        process (clk, rst)
        begin
            -- New reset for the write-first logic.
            if (rst = '1') then
                bypass_valid_r <= '0';
            elsif (rising_edge(clk)) then
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
            process (clk, rst)
            begin
                -- New reset for the registered read data. 
                -- IMPORTANT: I would avoid this reset unless absolutely 
                -- necessary. Just like some FPGAs don't support a read enable
                -- on this register, some don't support a reset at all, some
                -- only support an async reset, some only support a sync reset,
                -- etc. Your design will always still work with this reset, but
                -- it may prevent synthesis from packing the register into the
                -- RAM resource. You might not care, but when doing timing
                -- optimization, you might need to specialize your template for
                -- your specific FPGA. You could also go crazy with more 
                -- if-generate combinations, which is something I have in my 
                -- personal templates, but I've also been doing this for decades.
                -- I don't recommend adding a ton of functionality to your 
                -- template until you know you are going to use it.
                if (rst = '1') then
                    rd_data <= (others => '0');
                elsif (rising_edge(clk)) then
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
            process (clk, rst)
            begin
                -- New reset for the registered read data. See above comment.
                -- I avoid this reset whenever possible.
                if (rst = '1') then
                    rd_data <= (others => '0');
                elsif (rising_edge(clk)) then
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
