LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE std.env.finish;

ENTITY full_adder_1bit_tb IS
END ENTITY;

ARCHITECTURE Behavioral OF full_adder_1bit_tb IS

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
  SIGNAL a_in_tb    : STD_LOGIC := '0';
  SIGNAL b_in_tb    : STD_LOGIC := '0';
  SIGNAL c_in_tb    : STD_LOGIC := '0';
  SIGNAL sum_out_tb : STD_LOGIC := '0';
  SIGNAL c_out_tb   : STD_LOGIC := '0';

  --1/25MHz => 40ns
  CONSTANT clock_period_tb : TIME := 40 ns;

BEGIN
  --Instantiate components
  adder :
  full_adder_1bit PORT MAP(
    a     => a_in_tb,
    b     => b_in_tb,
    c_in  => c_in_tb,
    sum   => sum_out_tb,
    c_out => c_out_tb
  );

  PROCESS
    VARIABLE input_vec_tb : STD_LOGIC_VECTOR(2 DOWNTO 0);
  BEGIN
    FOR i IN 0 TO 7 LOOP
      input_vec_tb := STD_LOGIC_VECTOR(TO_UNSIGNED(i, input_vec_tb'length));

      a_in_tb <= input_vec_tb(0);
      b_in_tb <= input_vec_tb(1);
      c_in_tb <= input_vec_tb(2);

      WAIT FOR clock_period_tb;
    END LOOP;

    WAIT FOR clock_period_tb;
    finish;

  END PROCESS;

END Behavioral;
