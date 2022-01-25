LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BarrelShifterGeneric IS
  GENERIC (N : NATURAL RANGE 2 TO 128);
  PORT (
    d_in  : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    d_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    rot   : IN INTEGER RANGE 0 TO N - 1);
END;

ARCHITECTURE Behavioural OF BarrelShifterGeneric IS
BEGIN
  d_out <= d_in ROR rot;
END;
