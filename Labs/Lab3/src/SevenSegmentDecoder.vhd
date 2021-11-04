LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY sev_segment IS
  PORT (
    display_value  : IN INTEGER RANGE 0 TO 9;
    segment_select : OUT STD_LOGIC;
    segment_output : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
END sev_segment;

ARCHITECTURE Behavioral OF sev_segment IS
BEGIN

  segment_select <= '1';

  WITH display_value SELECT segment_output <=
    "1111110" WHEN 0,
    "0110000" WHEN 1,
    "1101101" WHEN 2,
    "1111001" WHEN 3,
    "0110011" WHEN 4,
    "1011011" WHEN 5,
    "1011111" WHEN 6,
    "1110000" WHEN 7,
    "1111111" WHEN 8,
    "1110011" WHEN 9,
    "1000000" WHEN OTHERS;

END ARCHITECTURE;
