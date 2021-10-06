library IEEE;
use IEEE.std_logic_1164.all;

entity GrayCounter_tb is

end;

ARCHITECTURE behavioral OF GrayCounter_tb  IS
  COMPONENT GrayCounter
	    PORT (
        clk: IN STD_LOGIC;
		  output: OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
  END COMPONENT;
  signal clk_tb : STD_LOGIC := '0';
  signal output_tb : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";
  begin
  
  counter: GrayCounter port map( 
		clk => clk_tb,
		output => output_tb);
	clock:
	PROCESS
	BEGIN
	clk_tb <= '0';
	WAIT FOR 40ns;
	clk_tb <= '1';
	WAIT FOR 40ns;
	END PROCESS;
END Behavioral;