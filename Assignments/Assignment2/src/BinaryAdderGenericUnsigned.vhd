LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY BinaryAdderGenericUnsigned IS
  GENERIC (WID : NATURAL);
  PORT (
    a   : IN UNSIGNED(WID - 1 DOWNTO 0);
    b   : IN UNSIGNED(WID - 1 DOWNTO 0);
    c   : IN UNSIGNED(WID - 1 DOWNTO 0);
    sum : OUT UNSIGNED(WID + 1 DOWNTO 0) := (OTHERS => '0'));
END;

ARCHITECTURE Behavioural OF BinaryAdderGenericUnsigned IS
BEGIN
  PROCESS (a, b, c)
  BEGIN
    sum <= to_unsigned(to_integer(a) + to_integer(b) + to_integer(c), sum'length);
  END PROCESS;
END;
