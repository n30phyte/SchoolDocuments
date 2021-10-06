LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY full_adder_1bit IS
  PORT (
    a     : IN STD_LOGIC;
    b     : IN STD_LOGIC;
    c_in  : IN STD_LOGIC;
    sum   : OUT STD_LOGIC;
    c_out : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Dataflow OF full_adder_1bit IS
BEGIN

  sum   <= a XOR b XOR c_in;
  c_out <= (a AND b) OR (b AND c_in) OR (a AND c_in);

END ARCHITECTURE;
