LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ProgMem IS PORT (
  pm_addr : IN INTEGER RANGE 0 TO 31 := 0;
  data    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
);
END ENTITY;

ARCHITECTURE rtl OF ProgMem IS
  TYPE rom_type IS ARRAY (0 TO 31) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL ROM : rom_type := (
    "00110000",            -- LDI A,
    "00011110",            -- 30
    "00100000",            -- STA 0, A
    "00110000",            -- LDI A,
    "00101000",            -- 40
    "01000000",            -- ADD A, 0
    "00100001",            -- STA 1, A
    "00110000",            -- LDI A,
    "01100100",            -- 100
    "01010001",            -- SUB A, 1
    OTHERS => "10100000"); -- HALT
BEGIN
  PROCESS(pm_addr)
  BEGIN
    data <= ROM(pm_addr);
  END PROCESS;
END ARCHITECTURE;
