LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY S1_mux IS
  PORT (
    cin       : IN STD_LOGIC;
    select_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    y_s1      : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioral OF S1_mux IS

BEGIN
  PROCESS (cin, select_in)
  BEGIN
    CASE select_in IS
      WHEN "0000" => y_s1 <= '0';
      WHEN "0001" => y_s1 <= cin;
      WHEN "0010" => y_s1 <= '1';
      WHEN "0011" => y_s1 <= NOT cin;
      WHEN "0100" => y_s1 <= cin;
      WHEN "0101" => y_s1 <= '1';
      WHEN "0110" => y_s1 <= NOT cin;
      WHEN "0111" => y_s1 <= '0';
      WHEN "1000" => y_s1 <= '1';
      WHEN "1001" => y_s1 <= NOT cin;
      WHEN "1010" => y_s1 <= '0';
      WHEN "1011" => y_s1 <= cin;
      WHEN "1100" => y_s1 <= NOT cin;
      WHEN "1101" => y_s1 <= '0';
      WHEN "1110" => y_s1 <= cin;
      WHEN "1111" => y_s1 <= '1';
      WHEN OTHERS => y_s1 <= '0';
    END CASE;
  END PROCESS;

END Behavioral;
