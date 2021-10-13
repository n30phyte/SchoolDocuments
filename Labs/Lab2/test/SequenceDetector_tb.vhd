LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY SequenceDetector_tb IS
END ENTITY;

ARCHITECTURE Behavioural OF SequenceDetector_tb IS
  COMPONENT SequenceDetector IS PORT (
    CLK   : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    X     : IN STD_LOGIC;
    Z     : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL clk_tb   : STD_LOGIC;
  SIGNAL reset_tb : STD_LOGIC := '1';
  SIGNAL x_tb     : STD_LOGIC;
  SIGNAL z_tb     : STD_LOGIC;

  CONSTANT input_stimulus  : STD_LOGIC_VECTOR(0 TO 7) := "01101011";
  CONSTANT clock_period_tb : TIME                     := 40 ns; --1/25MHz => 40ns

BEGIN

  detector : SequenceDetector
  PORT MAP(
    clk   => clk_tb,
    reset => reset_tb,
    x     => x_tb,
    z     => z_tb);

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
        x_tb <= input_stimulus(current_idx);
        current_idx := current_idx + 1;
      ELSE
        std.env.finish;
      END IF;
    END IF;

  END PROCESS;

END ARCHITECTURE;
