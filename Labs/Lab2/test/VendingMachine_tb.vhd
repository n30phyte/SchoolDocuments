
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE STD.ENV.finish;

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

  SIGNAL clk_design           : STD_LOGIC                    := '0';
  SIGNAL rst                  : STD_LOGIC                    := '0';
  SIGNAL item_select          : STD_LOGIC                    := '0';
  SIGNAL coins                : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL change               : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL display              : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0000000";
  SIGNAL segment              : STD_LOGIC                    := '0';
  SIGNAL soft_drink_dispense  : STD_LOGIC                    := '0';
  SIGNAL granola_bar_dispense : STD_LOGIC                    := '0';

  CONSTANT clk_period : TIME := 20 ns;

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
    clk_design <= '1';
    WAIT FOR clk_period/2;
    clk_design <= '0';
    WAIT FOR clk_period/2;
  END PROCESS;

  stim_proc : PROCESS
  BEGIN
    rst <= '1';
    WAIT UNTIL falling_edge(clk_design);

    rst         <= '0';
    item_select <= '0';
    coins       <= "10";
    WAIT UNTIL falling_edge(clk_design);

    coins <= "00";
    WAIT UNTIL falling_edge(clk_design);

    ASSERT soft_drink_dispense = '1'
    REPORT "SOFT DRINK NOT DISPENSED"
      SEVERITY failure;
    WAIT UNTIL falling_edge(clk_design);

    coins <= "01";
    WAIT UNTIL falling_edge(clk_design);

    coins <= "01";
    WAIT UNTIL falling_edge(clk_design);

    coins <= "00";
    WAIT UNTIL falling_edge(clk_design);

    ASSERT soft_drink_dispense = '1'
    REPORT "SOFT DRINK NOT DISPENSED"
      SEVERITY failure;
    WAIT UNTIL falling_edge(clk_design);

    item_select <= '1';
    coins       <= "11";
    WAIT UNTIL falling_edge(clk_design);

    coins <= "11";
    WAIT UNTIL falling_edge(clk_design);

    ASSERT change = "10"
    REPORT "PROPER CHANGE NOT DISPENSED"
      SEVERITY failure;
    WAIT UNTIL falling_edge(clk_design);
    WAIT UNTIL falling_edge(clk_design);

    ASSERT granola_bar_dispense = '1'
    REPORT "GRANOLA BAR NOT DISPENSED"
      SEVERITY failure;

    WAIT UNTIL falling_edge(clk_design);

    item_select <= '1';
    coins       <= "11";
    WAIT UNTIL falling_edge(clk_design);

    coins <= "10";
    WAIT UNTIL falling_edge(clk_design);

    ASSERT change = "01"
    REPORT "PROPER CHANGE NOT DISPENSED"
      SEVERITY failure;
    WAIT UNTIL falling_edge(clk_design);
    WAIT UNTIL falling_edge(clk_design);
    WAIT UNTIL falling_edge(clk_design);

    finish;
  END PROCESS;
END ARCHITECTURE;
