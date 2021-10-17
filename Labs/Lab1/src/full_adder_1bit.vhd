--------------------------------------------------------------------------------
-- RTL implementation for 1 bit full adder
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-06
-- Target: GHDL, Quartus, Vivado
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY full_adder_1bit IS
  PORT (
    a     : IN STD_LOGIC;
    b     : IN STD_LOGIC;
    c_in  : IN STD_LOGIC;
    sum   : OUT STD_LOGIC;
    c_out : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE rtl OF full_adder_1bit IS
BEGIN

  sum   <= a XOR b XOR c_in;
  c_out <= (a AND b) OR (b AND c_in) OR (a AND c_in);

END ARCHITECTURE;
