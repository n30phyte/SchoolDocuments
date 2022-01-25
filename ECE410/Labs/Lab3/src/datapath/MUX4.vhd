LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY MUX4 IS PORT (
  sel        : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
  A, B, C, D : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  Y          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF MUX4 IS
BEGIN
  PROCESS (sel, A, B, C, D)
  BEGIN
    CASE(sel) IS
      WHEN "00" =>
      Y <= A;
      WHEN "01" =>
      Y <= B;
      WHEN "10" =>
      Y <= C;
      WHEN "11" =>
      Y <= D;
      WHEN OTHERS  =>
      Y <= (OTHERS => 'X');
    END CASE;
  END PROCESS;

END ARCHITECTURE;
