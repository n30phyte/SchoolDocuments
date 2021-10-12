LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY comparator IS
  PORT (
    a            : IN UNSIGNED(1 DOWNTO 0);
    b            : IN UNSIGNED(1 DOWNTO 0);
    greater_than : OUT STD_LOGIC;
    less_than    : OUT STD_LOGIC;
    equal        : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioral OF comparator IS
BEGIN

  PROCESS (a, b)
  BEGIN
    IF A = B THEN
      equal        <= '1';
      greater_than <= '0';
      less_than    <= '0';
    ELSIF A < B THEN
      equal        <= '0';
      greater_than <= '0';
      less_than    <= '1';
    ELSIF A > B THEN
      equal        <= '0';
      greater_than <= '1';
      less_than    <= '0';
    END IF;
  END PROCESS;

END ARCHITECTURE;
