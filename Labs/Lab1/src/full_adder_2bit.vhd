
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE std.env.finish;

ENTITY full_adder_2bit IS
  PORT (
    a     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    b     : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_in  : IN STD_LOGIC;
    sum   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    c_out : OUT STD_LOGIC
  );
END ENTITY;

ARCHITECTURE Structural OF full_adder_2bit IS

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

END Structural;
