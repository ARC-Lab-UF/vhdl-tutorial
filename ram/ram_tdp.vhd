-- Greg Stitt
-- StittHub (www.stitt-hub.com)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_tdp is
    generic (
        DATA_WIDTH  : positive := 16;
        ADDR_WIDTH  : positive := 10;
        REG_RD_DATA : boolean  := false;
        STYLE       : string   := ""
        );
    port (
        clk : in std_logic;

        -- Port A
        en_a      : in  std_logic;
        wr_en_a   : in  std_logic;
        addr_a    : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data_a : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_data_a : out std_logic_vector(DATA_WIDTH-1 downto 0);

        -- Port B
        en_b      : in  std_logic;
        wr_en_b   : in  std_logic;
        addr_b    : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
        wr_data_b : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_data_b : out std_logic_vector(DATA_WIDTH-1 downto 0)
        );
end ram_tdp;

architecture default_arch of ram_tdp is
    type ram_t is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal ram                : ram_t;
    -- Tell Quartus what type of RAM to use
    attribute ramstyle        : string;
    attribute ramstyle of ram : signal is STYLE;

    -- Tell Vivado what type of RAM to use
    attribute ram_style        : string;
    attribute ram_style of ram : signal is STYLE;

    signal rd_data_ram_a, rd_data_ram_b : std_logic_vector(DATA_WIDTH-1 downto 0);
begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (en_a = '1') then
                if (wr_en_a = '1') then
                    ram(to_integer(unsigned(addr_a))) <= wr_data_a;
                else
                    rd_data_ram_a <= ram(to_integer(unsigned(addr_a)));
                end if;                
            end if;
        end if;
    end process;

    process (clk)
    begin
        if (rising_edge(clk)) then
            if (en_b = '1') then
                if (wr_en_b = '1') then
                    ram(to_integer(unsigned(addr_b))) <= wr_data_b;
                else
                    rd_data_ram_b <= ram(to_integer(unsigned(addr_b)));
                end if;                
            end if;
        end if;
    end process;

    l_reg_rd_data : if (REG_RD_DATA) generate
        process (clk)
        begin
            if (rising_edge(clk)) then
                if (en_a= '1') then
                    rd_data_a <= rd_data_ram_a;
                end if;
                if (en_b= '1') then
                    rd_data_b <= rd_data_ram_b;
                end if;
            end if;
        end process;
    end generate;

    l_no_reg_rd_data : if (not REG_RD_DATA) generate
        rd_data_a <= rd_data_ram_a;
        rd_data_b <= rd_data_ram_b;
    end generate;

end default_arch;
