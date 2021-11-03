LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY RegisterFile IS PORT (
  clk_rf    : IN STD_LOGIC;
  wr_rf     : IN STD_LOGIC;
  addr_rf   : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_rf  : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  output_rf : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF RegisterFile IS
  SUBTYPE reg IS STD_LOGIC_VECTOR(7 DOWNTO 0);
  TYPE regArray IS ARRAY(0 TO 7) OF reg;
  SIGNAL RF : regArray;

BEGIN
  PROCESS (clk_rf, wr_rf)
  BEGIN
    IF (clk_rf'event AND clk_rf = '1') THEN
      IF (wr_rf = '1') THEN
      ELSE
      END IF;
    END IF;
  END PROCESS;
END ARCHITECTURE;
