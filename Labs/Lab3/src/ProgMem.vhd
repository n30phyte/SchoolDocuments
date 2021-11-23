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
    "10000001",            -- IN A (Lower)
    "10000000",            -- IN A (Upper)
    "11000000",            -- JZ
    "00000101",            -- 05
    "01100010",            -- SHFL A, 2
    "10010000",            -- OUT A
    "00110000",            -- LDI A,
    "00001010",            -- 10
    "01110001",            -- SHFR A, 1
    "10010000",            -- OUT A
    OTHERS => "10100000"); -- HALT
  -- SIGNAL ROM : rom_type := (
  --   "10000000",            -- IN A (Upper)
  --   "10000001",            -- IN A (Lower)
  --   "00100000",            -- STA 0, A
  --   "00110000",            -- LDI A,
  --   "00101000",            -- 40
  --   "01000000",            -- ADD A, 0
  --   "00100001",            -- STA 1, A
  --   "00110000",            -- LDI A,
  --   "01100100",            -- 100
  --   "01010001",            -- SUB A, 1
  --   "01100001",            -- SHFL A, 1
  --   "01110010",            -- SHFR A, 2
  --   OTHERS => "10100000"); -- HALT
BEGIN
  PROCESS (pm_addr)
  BEGIN
    data <= ROM(pm_addr);
  END PROCESS;
END ARCHITECTURE;
