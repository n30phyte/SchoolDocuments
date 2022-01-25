LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.MATH_REAL.ALL;

ENTITY RegisterFile IS
  GENERIC (
    WID : POSITIVE);
  PORT (
    clk   : IN STD_LOGIC;
    we    : IN STD_LOGIC;
    addr  : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    D_in  : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    D_out : OUT STD_LOGIC_VECTOR(WID - 1 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF RegisterFile IS
  CONSTANT file_size : INTEGER := 8;
  TYPE register_file_t IS ARRAY(0 TO file_size - 1) OF STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
  SIGNAL register_file : register_file_t;
BEGIN

  PROCESS (clk)
  BEGIN
    IF rising_edge(clk) THEN
      IF we = '1' THEN
        register_file(to_integer(unsigned(addr))) <= D_in;
      END IF;
      D_out <= register_file(to_integer(unsigned(addr)));
    END IF;

  END PROCESS;
END ARCHITECTURE;
