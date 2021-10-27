--------------------------------------------------------------------------------
-- Top level file for Lab 2
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-20
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY lab2_top IS
  PORT (
    global_clk     : IN STD_LOGIC;
    reset          : IN STD_LOGIC;
    item_sel       : IN STD_LOGIC;
    coins_in       : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    change_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    display_sum    : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
    select_segment : OUT STD_LOGIC;
    soft_drink     : OUT STD_LOGIC;
    granola_bar    : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Structural OF lab2_top IS

  -- Component Definitions
  COMPONENT VendingMachine IS
    PORT (
      clk            : IN STD_LOGIC;
      reset          : IN STD_LOGIC;
      item_sel       : IN STD_LOGIC;
      coins_in       : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      change_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      display_sum    : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
      select_segment : OUT STD_LOGIC;
      soft_drink     : OUT STD_LOGIC;
      granola_bar    : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT SequenceDetector IS
    PORT (
      clk          : IN STD_LOGIC;
      reset        : IN STD_LOGIC;
      seq_in       : IN STD_LOGIC;
      detector_out : OUT STD_LOGIC);
  END COMPONENT;
  COMPONENT ClockDivider IS
    PORT (
      clk_in  : IN STD_LOGIC;
      clk_out : OUT STD_LOGIC
    );
  END COMPONENT;

  SIGNAL clk_divided : STD_LOGIC;
BEGIN

  divider : ClockDivider
  PORT MAP(
    clk_in  => global_clk,
    clk_out => clk_divided
  );

  --  detector : SequenceDetector
  --  PORT MAP(
  --    clk          => clk_divided,
  --    reset        => reset,
  --    seq_in       => seq_in,
  --    detector_out => detector_out);

  machine : VendingMachine
  PORT MAP(
    clk            => clk_divided,
    reset          => reset,
    item_sel       => item_sel,
    coins_in       => coins_in,
    change_out     => change_out,
    display_sum    => display_sum,
    select_segment => select_segment,
    soft_drink     => soft_drink,
    granola_bar    => granola_bar);

END ARCHITECTURE;
