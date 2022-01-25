--------------------------------------------------------------------------------
-- Top level file for Lab 1
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

ENTITY lab1_top IS
  PORT (
    a           : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    b           : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_in        : IN STD_LOGIC;
    sum         : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_out       : OUT STD_LOGIC;
    comp_result : OUT STD_LOGIC_VECTOR(2 DOWNTO 0)
  );
END ENTITY;

ARCHITECTURE Structural OF lab1_top IS

  -- Component Definitions
  COMPONENT full_adder_2bit IS
    PORT (
      a     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      b     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      c_in  : IN STD_LOGIC;
      sum   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      c_out : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT comparator IS
    PORT (
      a            : IN UNSIGNED(1 DOWNTO 0);
      b            : IN UNSIGNED(1 DOWNTO 0);
      greater_than : OUT STD_LOGIC;
      less_than    : OUT STD_LOGIC;
      equal        : OUT STD_LOGIC
    );
  END COMPONENT;

BEGIN

  adder :
  full_adder_2bit PORT MAP(
    a     => a,
    b     => b,
    c_in  => c_in,
    sum   => sum,
    c_out => c_out
  );

  comp_map :
  comparator PORT MAP(
    a            => UNSIGNED(a),
    b            => UNSIGNED(b),
    greater_than => comp_result(2),
    less_than    => comp_result(1),
    equal        => comp_result(0)
  );

END ARCHITECTURE;

CONFIGURATION config_adder OF lab1_top IS
  FOR Structural
    FOR ALL : full_adder_2bit
      USE ENTITY work.full_adder_2bit(MUX);
    END FOR;
  END FOR;
END CONFIGURATION;
