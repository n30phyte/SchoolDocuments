-- Code your testbench here
library IEEE;
use IEEE.std_logic_1164.all;

entity BarrelShifter_tb is

end;

ARCHITECTURE behavioral OF BarrelShifter_tb IS
  COMPONENT BarrelShifter
	port (
    	d_in : in STD_LOGIC_VECTOR(7 downto 0);
    	d_out: out STD_LOGIC_VECTOR(7 downto 0);
    	rot: in integer range 0 to 7);
  END COMPONENT;
  signal input : STD_LOGIC_VECTOR(7 DOWNTO 0) := "01101101";
  signal output : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
  signal rot_amt : integer range 0 to 7 := 0;
  begin
  
  shifter: BarrelShifter port map( 
  	d_in => input,
    d_out => output,
    rot => rot_amt);
	 
  PROCESS
  BEGIN
	 
	 rot_amt <= 0;
	 wait for 10ns;
	 rot_amt <= 1;
	 wait for 10ns;
	 rot_amt <= 2;
	 wait for 10ns;
	 rot_amt <= 3;
	 wait for 10ns;
	 rot_amt <= 4;
	 wait for 10ns;
	 rot_amt <= 5;
	 wait for 10ns;
	 rot_amt <= 6;
	 wait for 10ns;
	 rot_amt <= 7;
	 wait for 10ns;
	 
	END PROCESS;
END Behavioral;