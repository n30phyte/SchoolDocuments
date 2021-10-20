
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY VendingMachine_tb IS
END ENTITY;

ARCHITECTURE Behavioral OF VendingMachine_tb IS

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

  SIGNAL clk_design           : STD_LOGIC;
  SIGNAL rst                  : STD_LOGIC;
  SIGNAL item_select          : STD_LOGIC;
  SIGNAL coins                : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL change               : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL display              : STD_LOGIC_VECTOR(6 DOWNTO 0);
  SIGNAL segment              : STD_LOGIC;
  SIGNAL soft_drink_dispense  : STD_LOGIC;
  SIGNAL granola_bar_dispense : STD_LOGIC;
  SIGNAL dispensed            : STD_LOGIC_VECTOR(1 DOWNTO 0);

  CONSTANT clk_period : TIME := 40 ns;

BEGIN
  VENDING_ENT : VendingMachine PORT MAP(
    clk            => clk_design,
    reset          => rst,
    item_sel       => item_select,
    coins_in       => coins,
    change_out     => change,
    display_sum    => display,
    select_segment => segment,
    soft_drink     => soft_drink_dispense,
    granola_bar    => granola_bar_dispense);

  clk_process : PROCESS
  BEGIN
    clk_design <= '0';
    WAIT FOR clk_period/2;
    clk_design <= '1';
    WAIT FOR clk_period/2;
  END PROCESS;

  stim_proc : PROCESS
  BEGIN

    rst <= '0';
  END PROCESS;
END ARCHITECTURE;
