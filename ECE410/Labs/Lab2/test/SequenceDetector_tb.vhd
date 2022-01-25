--------------------------------------------------------------------------------
-- Testbench for Mealy Machine Sequence Detector
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-20
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

USE std.env.finish;

ENTITY SequenceDetector_tb IS
END ENTITY;

ARCHITECTURE Behavioural OF SequenceDetector_tb IS
  COMPONENT SequenceDetector IS PORT (
    clk          : IN STD_LOGIC;
    reset        : IN STD_LOGIC;
    seq_in       : IN STD_LOGIC;
    detector_out : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL clk_tb          : STD_LOGIC;
  SIGNAL reset_tb        : STD_LOGIC := '1';
  SIGNAL seq_in_tb       : STD_LOGIC := '0';
  SIGNAL detector_out_tb : STD_LOGIC;

  CONSTANT input_stimulus : STD_LOGIC_VECTOR(0 TO 22) := "01101101110110001101011";

  --1/25MHz => 40ns
  CONSTANT clock_period_tb : TIME := 40 ns;

BEGIN

  detector : SequenceDetector
  PORT MAP(
    clk          => clk_tb,
    reset        => reset_tb,
    seq_in       => seq_in_tb,
    detector_out => detector_out_tb);

  clock :
  PROCESS
  BEGIN
    clk_tb <= '0';
    WAIT FOR clock_period_tb/2;
    clk_tb <= '1';
    WAIT FOR clock_period_tb/2;
  END PROCESS;

  stimulus : PROCESS (clk_tb)
    VARIABLE current_idx : INTEGER := 0;
  BEGIN
    reset_tb <= '0';
    IF rising_edge(clk_tb) THEN
      IF current_idx < input_stimulus'length THEN
        seq_in_tb <= input_stimulus(current_idx);
        current_idx := current_idx + 1;
      ELSE
        finish;
      END IF;
    END IF;

  END PROCESS;
END ARCHITECTURE;
