LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY sev_segment IS
  PORT (
    --output of PC from cpu
    DispVal : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
    anode   : OUT STD_LOGIC;
    --controls which digit to display
    segOut : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
END sev_segment;

ARCHITECTURE Behavioral OF sev_segment IS
BEGIN

  anode <= '1';

  WITH DispVal SELECT
    segOut <= "0111111" WHEN "00000",
    "0000110" WHEN "00001",
    "1011011" WHEN "00010",
    "1001111" WHEN "00011",
    "1100110" WHEN "00100",
    "1101101" WHEN "00101",
    "1111101" WHEN "00110",
    "0000111" WHEN "00111",
    "1111111" WHEN "01000",
    "1101111" WHEN "01001",

    "1000000" WHEN OTHERS;

END Behavioral;
