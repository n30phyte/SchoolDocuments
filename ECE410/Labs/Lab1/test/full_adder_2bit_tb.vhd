--------------------------------------------------------------------------------
-- Testbench for 2 bit Full Adder
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-04
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
      a           : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      b           : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      c_in        : IN STD_LOGIC;
      sum         : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      c_out       : OUT STD_LOGIC;
      comp_result : OUT STD_LOGIC_VECTOR(2 DOWNTO 0)
    );
  END COMPONENT;

  -- Signal Definitions
  SIGNAL a_in_tb        : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL b_in_tb        : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL c_in_tb        : STD_LOGIC                    := '0';
  SIGNAL sum_out_tb     : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";
  SIGNAL c_out_tb       : STD_LOGIC                    := '0';
  SIGNAL comp_result_tb : STD_LOGIC_VECTOR(2 DOWNTO 0) := "000";

  --1/25MHz => 40ns
  CONSTANT clock_period_tb : TIME := 40 ns;

BEGIN
  --Instantiate components
  adder : full_adder_2bit
  PORT MAP(
    a           => a_in_tb,
    b           => b_in_tb,
    c_in        => c_in_tb,
    sum         => sum_out_tb,
    c_out       => c_out_tb,
    comp_result => comp_result_tb
  );

  PROCESS
    VARIABLE input_vec_tb   : UNSIGNED(4 DOWNTO 0);
    VARIABLE expected_sum   : INTEGER;
    VARIABLE calculated_sum : UNSIGNED(2 DOWNTO 0);

  BEGIN
    FOR i IN 0 TO 31 LOOP
      input_vec_tb := TO_UNSIGNED(i, input_vec_tb'length);

      expected_sum := TO_INTEGER(input_vec_tb(1 DOWNTO 0)) + TO_INTEGER(input_vec_tb(3 DOWNTO 2)) + TO_INTEGER(unsigned'("" & input_vec_tb(4)));

      a_in_tb <= STD_LOGIC_VECTOR(input_vec_tb(1 DOWNTO 0));
      b_in_tb <= STD_LOGIC_VECTOR(input_vec_tb(3 DOWNTO 2));
      c_in_tb <= STD_LOGIC(input_vec_tb(4));

      WAIT FOR clock_period_tb;

      calculated_sum(2)          := c_out_tb;
      calculated_sum(1 DOWNTO 0) := UNSIGNED(sum_out_tb);

      ASSERT TO_INTEGER(calculated_sum) = expected_sum
      REPORT "Incorrect Answer. Expected: " & INTEGER'image(expected_sum) & ". Got: " & INTEGER'image(TO_INTEGER(calculated_sum))
      SEVERITY failure;
    END LOOP;

    WAIT FOR clock_period_tb;
    finish;

  END PROCESS;

END ARCHITECTURE;

CONFIGURATION config_adder OF full_adder_2bit_tb IS
  FOR Behavioral
    FOR ALL : full_adder_2bit
      USE ENTITY work.full_adder_2bit(MUX);
    END FOR;
  END FOR;
END CONFIGURATION;
