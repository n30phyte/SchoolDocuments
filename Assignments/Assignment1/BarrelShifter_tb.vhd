-- Code your testbench here
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BarrelShifter_tb IS

END;

ARCHITECTURE behavioral OF BarrelShifter_tb IS
  COMPONENT BarrelShifter
    PORT (
      d_in  : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      d_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      rot   : IN INTEGER RANGE 0 TO 7);
  END COMPONENT;
  SIGNAL input   : STD_LOGIC_VECTOR(7 DOWNTO 0) := "01101101";
  SIGNAL output  : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
  SIGNAL rot_amt : INTEGER RANGE 0 TO 7         := 0;
BEGIN

  shifter : BarrelShifter PORT MAP(
    d_in  => input,
    d_out => output,
    rot   => rot_amt);

  PROCESS
  BEGIN

    rot_amt <= 0;
    WAIT FOR 10ns;
    rot_amt <= 1;
    WAIT FOR 10ns;
    rot_amt <= 2;
    WAIT FOR 10ns;
    rot_amt <= 3;
    WAIT FOR 10ns;
    rot_amt <= 4;
    WAIT FOR 10ns;
    rot_amt <= 5;
    WAIT FOR 10ns;
    rot_amt <= 6;
    WAIT FOR 10ns;
    rot_amt <= 7;
    WAIT FOR 10ns;

  END PROCESS;
END Behavioral;
