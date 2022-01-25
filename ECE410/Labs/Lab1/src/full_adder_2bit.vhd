--------------------------------------------------------------------------------
-- RTL implementation for 2 bit full adder
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

ENTITY full_adder_2bit IS
  PORT (
    a     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    b     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_in  : IN STD_LOGIC;
    sum   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_out : OUT STD_LOGIC
  );
END ENTITY;

ARCHITECTURE RTL OF full_adder_2bit IS

  -- Component Definitions
  COMPONENT full_adder_1bit IS
    PORT (
      a     : IN STD_LOGIC;
      b     : IN STD_LOGIC;
      c_in  : IN STD_LOGIC;
      sum   : OUT STD_LOGIC;
      c_out : OUT STD_LOGIC
    );
  END COMPONENT;

  -- Signal Definitions
  SIGNAL c_temp : STD_LOGIC := '0';

BEGIN
  --Instantiate components
  adder1 :
  full_adder_1bit PORT MAP(
    a     => a(0),
    b     => b(0),
    c_in  => c_in,
    sum   => sum(0),
    c_out => c_temp
  );

  adder2 :
  full_adder_1bit PORT MAP(
    a     => a(1),
    b     => b(1),
    c_in  => c_temp,
    sum   => sum(1),
    c_out => c_out
  );

END ARCHITECTURE;

ARCHITECTURE MUX OF full_adder_2bit IS

  COMPONENT Co_mux IS
    PORT (
      cin       : IN STD_LOGIC;
      select_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
      y_cout    : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT S1_mux IS
    PORT (
      cin       : IN STD_LOGIC;
      select_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
      y_s1      : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT S0_mux IS
    PORT (
      cin       : IN STD_LOGIC;
      select_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
      y_s0      : OUT STD_LOGIC);
  END COMPONENT;

BEGIN

  carry_map : Co_mux PORT MAP(
    cin                   => C_in,
    select_in(3 DOWNTO 2) => A,
    select_in(1 DOWNTO 0) => B,
    y_cout                => c_out);

  s1_map : S1_mux PORT MAP(
    cin                   => C_in,
    select_in(3 DOWNTO 2) => A,
    select_in(1 DOWNTO 0) => B,
    y_s1                  => sum(1));

  s0_map : S0_mux PORT MAP(
    cin                   => C_in,
    select_in(3 DOWNTO 2) => A,
    select_in(1 DOWNTO 0) => B,
    y_s0                  => sum(0));

END ARCHITECTURE;
