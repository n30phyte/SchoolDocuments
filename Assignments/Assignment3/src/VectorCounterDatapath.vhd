LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY VectorCounterDatapath IS
  PORT (
    A               : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    is_odd_parity   : OUT STD_LOGIC;
    has_one_triplet : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Structural OF VectorCounterDatapath IS
  COMPONENT TripletChecker IS
    PORT (
      A : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      Y : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT ParityChecker IS
    PORT (
      A : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      Y : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL triplet_checker_output : STD_LOGIC;
BEGIN

  triplet : COMPONENT TripletChecker PORT MAP(
    A => A,
    Y => has_one_triplet);

  parity : COMPONENT ParityChecker PORT MAP(
    A => A,
    Y => is_odd_parity);

END ARCHITECTURE;
