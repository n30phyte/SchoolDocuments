LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BinaryAdderGeneric IS
  GENERIC (WID : NATURAL);
  PORT (
    a   : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    b   : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    c   : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    sum : OUT STD_LOGIC_VECTOR(WID + 1 DOWNTO 0) := (OTHERS => '0'));
END;

ARCHITECTURE Behavioural_Loop OF BinaryAdderGeneric IS
BEGIN

  PROCESS (a, b, c)
    VARIABLE temp_sum : STD_LOGIC_VECTOR(WID + 1 DOWNTO 0) := (OTHERS => '0');
  BEGIN
    FOR i IN 0 TO WID - 1 LOOP

      IF i = 0 THEN
        temp_sum(i) := a(i) XOR b(i) XOR c(i);
      ELSE
        temp_sum(i) := a(i) XOR b(i) XOR c(i) XOR temp_sum(i);
      END IF;

      temp_sum(i + 1) := (a(i) AND b(i)) OR (b(i) AND c(i)) OR (a(i) AND c(i));
    END LOOP;
    sum <= temp_sum;
  END PROCESS;
END;
