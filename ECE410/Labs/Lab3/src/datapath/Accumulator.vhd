--------------------------------------------------------------------------------
-- CPU Accumulator
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-11-02
-- 
-- Written in VHDL 2008
-- Entity Description:
-- Reset: Synchronous
-- Rising Edge only
-- clk: Clock
-- rst: Active high reset
-- we: Write Enable
-- D_in: Data Input
-- D_out: Data Output
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Accumulator IS PORT (
  clk   : IN STD_LOGIC;
  rst   : IN STD_LOGIC;
  we    : IN STD_LOGIC;
  D_in  : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
  D_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF Accumulator IS

BEGIN

  PROCESS (clk)
  BEGIN

    IF rising_edge(clk) THEN
      IF rst = '1' THEN
        D_out <= (OTHERS => '0');
      ELSIF we = '1' THEN
        D_out <= D_in;
      END IF;
    END IF;

  END PROCESS;

END ARCHITECTURE;
