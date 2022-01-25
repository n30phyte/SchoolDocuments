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
     "10000001",            -- IN A (Upper)
     "10000000",            -- IN A (lower)
     "11000000",            -- JZ
     "00000101",            -- 05
     "01100010",            -- SHFL A, 2
     "10010000",            -- OUT A
     "00110000",            -- LDI A,
     "00001010",            -- 10
     "01110001",            -- SHFR A, 1
     "10010000",            -- OUT A
     "10100000",            -- HALT
     OTHERS => "00000000"); -- NOP
--    "10000001",            -- IN A (Upper)
--    "10000000",            -- IN A (Lower)
--    "00100100",            -- STA R[4], A
--    "00110000",            -- LDI A,
--    "00001111",            -- 15
--    "01010100",            -- SUB A, R[4]
--    "10010000",            -- OUT A
--    "10100000",            -- HALT
--    OTHERS => "00000000"); -- NOP
BEGIN
  PROCESS (pm_addr)
  BEGIN
    data <= ROM(pm_addr);
  END PROCESS;
END ARCHITECTURE;
