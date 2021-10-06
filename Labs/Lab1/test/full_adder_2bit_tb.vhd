--------------------------------------------------------------------------------
-- Testbench for 2 bit Full Adder
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-06-10
-- Target: GHDL, Quartus, Vivado
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE std.env.finish;

ENTITY full_adder_2bit_tb IS
END ENTITY;

ARCHITECTURE Behavioral OF full_adder_2bit_tb IS

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

  -- Signal Definitions
  SIGNAL a_in_tb    : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL b_in_tb    : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL c_in_tb    : STD_LOGIC                    := '0';
  SIGNAL sum_out_tb : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL c_out_tb   : STD_LOGIC                    := '0';

  --1/25MHz => 40ns
  CONSTANT clock_period_tb : TIME := 40 ns;

BEGIN
  --Instantiate components
  adder :
  full_adder_2bit PORT MAP(
    a     => a_in_tb,
    b     => b_in_tb,
    c_in  => c_in_tb,
    sum   => sum_out_tb,
    c_out => c_out_tb
  );

  PROCESS
    VARIABLE input_vec_tb   : UNSIGNED(4 DOWNTO 0);
    VARIABLE expected_sum   : UNSIGNED(2 DOWNTO 0);
    VARIABLE calculated_sum : UNSIGNED(2 DOWNTO 0);

  BEGIN
    FOR i IN 0 TO 31 LOOP
      input_vec_tb := TO_UNSIGNED(i, input_vec_tb'length);

      expected_sum := input_vec_tb(1 DOWNTO 0) + input_vec_tb(3 DOWNTO 2) + unsigned'('0' & input_vec_tb(4));

      a_in_tb <= STD_LOGIC_VECTOR(input_vec_tb(1 DOWNTO 0));
      b_in_tb <= STD_LOGIC_VECTOR(input_vec_tb(3 DOWNTO 2));
      c_in_tb <= STD_LOGIC(input_vec_tb(4));

      WAIT FOR clock_period_tb;

      calculated_sum(2) := c_out_tb;
      calculated_sum(1) := sum_out_tb;

      ASSERT calculated_sum = expected_sum
      REPORT "Incorrect Answer. Expected: " & INTEGER'image(TO_INTEGER(expected_sum)) & ". Got: " & INTEGER'image(TO_INTEGER(calculated_sum))
      SEVERITY failure;

    END LOOP;

    WAIT FOR clock_period_tb;
    finish;

  END PROCESS;

END Behavioral;
