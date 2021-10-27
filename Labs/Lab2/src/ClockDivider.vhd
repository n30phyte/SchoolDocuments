--------------------------------------------------------------------------------
-- Clock Divider for ECE 410
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-20
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY ClockDivider IS
  PORT (
    clk_in  : IN STD_LOGIC;
    clk_out : OUT STD_LOGIC
  );
END ENTITY;

ARCHITECTURE behavioral OF ClockDivider IS
  SIGNAL count : INTEGER := 1;
BEGIN

  PROCESS (clk_in)
  BEGIN
    IF rising_edge(clk_in) THEN
      count <= count + 1;
      IF count = 62500000 THEN
        clk_out <= NOT clk_out;
        count   <= 1;
      END IF;
    END IF;
  END PROCESS;

END ARCHITECTURE;
