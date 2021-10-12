-- Code your design here
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BarrelShifter IS
  PORT (
    d_in  : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    d_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    rot   : IN INTEGER RANGE 0 TO 7);
END;

ARCHITECTURE Behavioural OF BarrelShifter IS
BEGIN
  WITH rot SELECT d_out <=
    d_in WHEN 0,
    d_in(0) & d_in(7 DOWNTO 1) WHEN 1,
    d_in(1 DOWNTO 0) & d_in(7 DOWNTO 2) WHEN 2,
    d_in(2 DOWNTO 0) & d_in(7 DOWNTO 3) WHEN 3,
    d_in(3 DOWNTO 0) & d_in(7 DOWNTO 4) WHEN 4,
    d_in(4 DOWNTO 0) & d_in(7 DOWNTO 5) WHEN 5,
    d_in(5 DOWNTO 0) & d_in(7 DOWNTO 6) WHEN 6,
    d_in(6 DOWNTO 0) & d_in(7) WHEN 7;

END;
