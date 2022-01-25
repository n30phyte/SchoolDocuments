--------------------------------------------------------------------------------
-- Simple comparator implementation
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-06
-- Target: GHDL, Quartus, Vivado
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY comparator IS
  PORT (
    a            : IN UNSIGNED(1 DOWNTO 0);
    b            : IN UNSIGNED(1 DOWNTO 0);
    greater_than : OUT STD_LOGIC;
    less_than    : OUT STD_LOGIC;
    equal        : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioral OF comparator IS
BEGIN
  equal        <= '1' WHEN A = B ELSE '0';
  greater_than <= '1' WHEN A > B ELSE '0';
  less_than    <= '1' WHEN A < B ELSE '0';
END ARCHITECTURE;
