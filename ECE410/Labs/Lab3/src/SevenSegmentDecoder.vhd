LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY SevenSegmentDecoder IS
  PORT (
    display_value  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    segment_select : OUT STD_LOGIC;
    segment_output : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioral OF SevenSegmentDecoder IS
BEGIN

  segment_select <= '1';

  WITH display_value SELECT segment_output <=
    "1111110" WHEN "00000",
    "0110000" WHEN "00001",
    "1101101" WHEN "00010",
    "1111001" WHEN "00011",
    "0110011" WHEN "00100",
    "1011011" WHEN "00101",
    "1011111" WHEN "00110",
    "1110000" WHEN "00111",
    "1111111" WHEN "01000",
    "1110011" WHEN "01001",
    "1000000" WHEN OTHERS;

END ARCHITECTURE;
