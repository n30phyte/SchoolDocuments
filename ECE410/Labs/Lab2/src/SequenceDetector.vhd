--------------------------------------------------------------------------------
-- Overlapping sequence detector for pattern '11011' with Mealy FSM
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-20
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY SequenceDetector IS
  PORT (
    clk          : IN STD_LOGIC;
    reset        : IN STD_LOGIC;
    seq_in       : IN STD_LOGIC;
    detector_out : OUT STD_LOGIC := '0');
END ENTITY;

ARCHITECTURE Behavioural OF SequenceDetector IS
  TYPE SEQUENCE_STATE IS (A, B, C, D, E);
  SIGNAL state_current : SEQUENCE_STATE;
  SIGNAL state_next    : SEQUENCE_STATE;
BEGIN

  PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      state_current <= A;
    ELSIF rising_edge(clk) THEN
      state_current <= state_next;
    END IF;
  END PROCESS;

  PROCESS (state_current, seq_in)
  BEGIN
    CASE state_current IS
      WHEN A =>
        IF seq_in = '0' THEN
          state_next   <= A;
          detector_out <= '0';
        ELSE
          state_next   <= B;
          detector_out <= '0';
        END IF;
      WHEN B =>
        IF seq_in = '0' THEN
          state_next   <= A;
          detector_out <= '0';
        ELSE
          state_next   <= C;
          detector_out <= '0';
        END IF;
      WHEN C =>
        IF seq_in = '0' THEN
          state_next   <= D;
          detector_out <= '0';
        ELSE
          state_next   <= C;
          detector_out <= '0';
        END IF;
      WHEN D =>
        IF seq_in = '0' THEN
          state_next   <= A;
          detector_out <= '0';
        ELSE
          state_next   <= E;
          detector_out <= '0';
        END IF;
      WHEN E =>
        IF seq_in = '0' THEN
          state_next   <= A;
          detector_out <= '0';
        ELSE
          state_next   <= C;
          detector_out <= '1';
        END IF;
      WHEN OTHERS =>
        state_next   <= A;
        detector_out <= '0';
    END CASE;
  END PROCESS;
END ARCHITECTURE;
