LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BinaryAdderGeneric IS
  GENERIC (WID : NATURAL);
  PORT (
    a         : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    b         : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    carry_in  : IN STD_LOGIC;
    sum       : OUT STD_LOGIC_VECTOR(WID - 1 DOWNTO 0) := (OTHERS => '0');
    carry_out : OUT STD_LOGIC;
  );
END;

ARCHITECTURE Behavioural OF BinaryAdderGeneric IS
BEGIN

  PROCESS (a, b)

  BEGIN
    adder_array : FOR i IN 0 TO WID - 1 GENERATE

    END GENERATE;
  END PROCESS;
END;
