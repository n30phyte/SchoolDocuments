----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Shyama Gandhi
-- 
-- Create Date: 07/29/2020 08:32:34 AM
-- Design Name: 
-- Module Name: bcd_tb - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE std.env.finish;

ENTITY bcd_counter_tb IS

END bcd_counter_tb;

ARCHITECTURE behavioral OF bcd_counter_tb IS
  --Include Components
  COMPONENT bcd_counter
    PORT (
      clk   : IN STD_LOGIC;
      dir   : IN STD_LOGIC;
      reset : IN STD_LOGIC;
      led   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT clk_divider IS
    PORT (
      clk_in  : IN STD_LOGIC;
      clk_out : OUT STD_LOGIC);
  END COMPONENT;

  --Input Signals
  SIGNAL clk_in_tb : STD_LOGIC                    := '0';
  SIGNAL reset_tb  : STD_LOGIC                    := '0';
  SIGNAL dir_tb    : STD_LOGIC                    := '1';
  SIGNAL load_tb   : STD_LOGIC                    := '0';
  SIGNAL count_in  : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');

  --Output Signal
  SIGNAL led_tb     : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL clk_out_tb : STD_LOGIC := '0';

  --Clock period definition
  CONSTANT clock_period_tb : TIME := 40 ns; --1/25MHz => 40ns

BEGIN
  --Instantiate components
  count1 :
  bcd_counter PORT MAP(
    clk   => clk_in_tb,
    dir   => dir_tb,
    reset => reset_tb,
    led   => led_tb
  );
  divider :
  clk_divider PORT MAP(
    clk_in  => clk_in_tb,
    clk_out => clk_out_tb
  );

  ----Clock process
  clock :
  PROCESS
  BEGIN
    clk_in_tb <= '0';
    WAIT FOR clock_period_tb/2;
    clk_in_tb <= '1';
    WAIT FOR clock_period_tb/2;
  END PROCESS;

  ----Stimulus process                
  PROCESS
  BEGIN
    reset_tb <= '1'; -- Reset the device under test and setup input to starting state
    WAIT FOR 40 ns;  -- Wait one clock period for our device to reset
    reset_tb <= '0';
    dir_tb   <= '1'; -- this for up counting in the BCD Counter
    -- Clock = 40ns * 2 = 80ns and the total number of counts = 10 so 10 * 80ns = 800ns atleast to see the whole range from 0 to 10.

    WAIT FOR 1000 ns;

    dir_tb <= '0';
    WAIT FOR 500 ns;

    --Asserts are followed by REPORT statement that prints a string report and then followed by severity clauses. ASSERT statements will check for a condition and upon failure of the condition
    --it will report a state.
    --Severity clause will tell you how serious the failed condition is.

    ASSERT(led_tb = "1001") -- set the value to 00 and nothing will
    REPORT "not zero"       -- appear in the tcl console window
      SEVERITY NOTE;
    finish;

  END PROCESS;

END Behavioral;
