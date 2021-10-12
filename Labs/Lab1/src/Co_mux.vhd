LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Co_mux IS
  PORT (
    cin       : IN STD_LOGIC;
    select_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    y_cout    : OUT STD_LOGIC);
END Co_mux;

ARCHITECTURE Behavioral OF Co_mux IS

BEGIN
  PROCESS (cin, select_in)
  BEGIN
    CASE select_in IS
      WHEN "0000" => y_cout <= '0';
      WHEN "0001" => y_cout <= '0';
      WHEN "0010" => y_cout <= '0';
      WHEN "0011" => y_cout <= cin;
      WHEN "0100" => y_cout <= '0';
      WHEN "0101" => y_cout <= '0';
      WHEN "0110" => y_cout <= cin;
      WHEN "0111" => y_cout <= '1';
      WHEN "1000" => y_cout <= '0';
      WHEN "1001" => y_cout <= cin;
      WHEN "1010" => y_cout <= '1';
      WHEN "1011" => y_cout <= '1';
      WHEN "1100" => y_cout <= cin;
      WHEN "1101" => y_cout <= '1';
      WHEN "1110" => y_cout <= '1';
      WHEN "1111" => y_cout <= '1';
      WHEN OTHERS => y_cout <= '0';
    END CASE;
  END PROCESS;

END Behavioral;
