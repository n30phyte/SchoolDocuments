----------------------------------------------------------------------------------
-- Company: Department of Electrical and Computer Engineering, University of Alberta
-- Engineer: Shyama Gandhi and Bruce Cockburn
--
-- Create Date: 10/29/2020 07:18:24 PM
-- Design Name: CONTROLLER FOR THE CPU
-- Module Name: 
-- Project Name:
-- Target Devices:
-- Tool Versions:
-- Description: CPU OF LAB 3 - ECE 410 (2020)
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--*********************************************************************************
-- This the seven segment that will display the current Program counter on any one of the two display(seven-segment)
----------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
ENTITY sev_segment IS
  PORT (
    --output of PC from cpu
    DispVal : IN STD_LOGIC_VECTOR (4downto 0);
    anode   : OUT STD_LOGIC;
    --controls which digit to display
    segOut : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
END sev_segment;

ARCHITECTURE Behavioral OF sev_segment IS
BEGIN

  anode <= '1';

  WITH DispVal SELECT
    segOut <= "0111111" WHEN "00000", --0
    "0000110" WHEN "00001",           --1
    "1011011" WHEN "00010",           --2
    "1001111" WHEN "00011",           --3
    "1100110" WHEN "00100",           --4
    "1101101" WHEN "00101",           --5
    "1111101" WHEN "00110",           --6
    "0000111" WHEN "00111",           --7
    "1111111" WHEN "01000",           --8
    "1101111" WHEN "01001",           --9

    -- ***************************************
    -- write the remaining lines to display from A to F, when "others" is provided to you...

    "1000000" WHEN OTHERS;

END Behavioral;
