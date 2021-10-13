LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY SequenceDetector IS
  PORT (
    CLK   : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    X     : IN STD_LOGIC;
    Z     : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioural OF SequenceDetector IS
  TYPE SEQUENCE_STATE IS (A, B, C, D, E, F);
BEGIN

  PROCESS (clk, reset)
    VARIABLE current_state : SEQUENCE_STATE := A;
  BEGIN

    IF RESET = '1' THEN
      current_state := A;
      Z <= '0';
    ELSIF rising_edge(clk) THEN
      CASE current_state IS
        WHEN A =>
          IF X = '0' THEN
            current_state := B;
          ELSE
            current_state := A;
          END IF;
        WHEN B =>
          IF X = '1' THEN
            current_state := C;
          ELSE
            current_state := A;
          END IF;
        WHEN C =>
          IF X = '1' THEN
            current_state := D;
          ELSE
            current_state := A;
          END IF;
        WHEN D =>
          IF X = '0' THEN
            current_state := E;
          ELSE
            current_state := A;
          END IF;
        WHEN E =>
          IF X = '1' THEN
            current_state := F;
          ELSE
            current_state := A;
          END IF;
        WHEN F =>
          Z <= '1';
          current_state                := A;
        WHEN OTHERS => current_state := A;
      END CASE;
    END IF;

  END PROCESS;
END ARCHITECTURE;
