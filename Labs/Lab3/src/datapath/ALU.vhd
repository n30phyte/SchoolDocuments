LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ALU IS PORT (
  op      : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
  A       : IN SIGNED(7 DOWNTO 0);
  B       : IN SIGNED(7 DOWNTO 0);
  rot_num : IN INTEGER RANGE 0 TO 3;
  Y       : OUT SIGNED (7 DOWNTO 0) := (OTHERS => '0'));
END ENTITY;

ARCHITECTURE Behavioural OF ALU IS
BEGIN
  PROCESS(A, B, rot_num, op)
  BEGIN
    CASE op IS
      WHEN "000" =>
        Y <= A;
      WHEN "001" =>
        Y <= A AND B;
      WHEN "010" =>
        Y <= shift_left(A, rot_num);
      WHEN "011" =>
        Y <= shift_right(A, rot_num);
      WHEN "100" =>
        Y <= A + B;
      WHEN "101" =>
        Y <= A - B;
      WHEN "110" =>
        Y <= A + 1;
      WHEN "111" =>
        Y <= A - 1;
      WHEN OTHERS  =>
        Y <= (OTHERS => 'X');
    END CASE;
  END PROCESS;
END ARCHITECTURE;
