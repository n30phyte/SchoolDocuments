-- Code your testbench here
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE std.env.finish;

ENTITY BarrelShifterGeneric_tb IS

END;

ARCHITECTURE Behavioural OF BarrelShifterGeneric_tb IS
  COMPONENT BarrelShifterGeneric
    GENERIC (N : NATURAL RANGE 2 TO 128);
    PORT (
      d_in  : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
      d_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
      rot   : IN INTEGER RANGE 0 TO N - 1);
  END COMPONENT;
  SIGNAL input   : STD_LOGIC_VECTOR(31 DOWNTO 0) := "01101101011011010110110101101101";
  SIGNAL output  : STD_LOGIC_VECTOR(31 DOWNTO 0) := "00000000000000000000000000000000";
  SIGNAL rot_amt : INTEGER RANGE 0 TO 31         := 0;
BEGIN

  shifter :
  BarrelShifterGeneric
  GENERIC MAP(N => 32)
  PORT MAP(
    d_in  => input,
    d_out => output,
    rot   => rot_amt);

  PROCESS
  BEGIN

    rot_amt <= 0;
    WAIT FOR 10 ns;
    rot_amt <= 1;
    WAIT FOR 10 ns;
    rot_amt <= 2;
    WAIT FOR 10 ns;
    rot_amt <= 3;
    WAIT FOR 10 ns;
    rot_amt <= 4;
    WAIT FOR 10 ns;
    rot_amt <= 5;
    WAIT FOR 10 ns;
    rot_amt <= 6;
    WAIT FOR 10 ns;
    rot_amt <= 7;
    WAIT FOR 10 ns;

    finish;

  END PROCESS;
END ARCHITECTURE;
