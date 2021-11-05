LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY BinaryAdderGeneric IS
  GENERIC (WID : NATURAL);
  PORT (
    A         : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    B         : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
    sum       : OUT STD_LOGIC_VECTOR(WID - 1 DOWNTO 0) := (OTHERS => '0');
    carry_out : OUT STD_LOGIC);
END;

ARCHITECTURE Behavioural OF BinaryAdderGeneric IS
  COMPONENT HalfAdder IS
    PORT (
      A     : IN STD_LOGIC;
      B     : IN STD_LOGIC;
      sum   : OUT STD_LOGIC;
      carry : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT FullAdder IS
    PORT (
      A         : IN STD_LOGIC;
      B         : IN STD_LOGIC;
      carry_in  : IN STD_LOGIC;
      sum       : OUT STD_LOGIC;
      carry_out : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL carry_temp : STD_LOGIC_VECTOR(7 DOWNTO 0);
BEGIN
  gen_loop : FOR i IN 0 TO WID - 1 GENERATE
    adders : IF i > 0 GENERATE
      full : COMPONENT FullAdder PORT MAP(
        A        => A(i),
        B        => B(i),
        carry_in => carry_temp(i - 1),
        sum      => sum(i),
        carry    => carry_temp(i)
      );
    ELSIF i = 0 GENERATE
        half : COMPONENT HalfAdder PORT MAP(
          A     => A(i),
          B     => B(i),
          sum   => sum(i),
          carry => carry_temp(i)
        );
      END GENERATE;
    END GENERATE;
  END;
