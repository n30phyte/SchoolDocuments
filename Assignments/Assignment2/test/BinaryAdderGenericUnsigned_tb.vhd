-- Code your testbench here
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE std.env.finish;

ENTITY BinaryAdderGenericUnsigned_tb IS

END;

ARCHITECTURE Behavioural OF BinaryAdderGenericUnsigned_tb IS

  COMPONENT BinaryAdderGenericUnsigned IS
    GENERIC (WID : NATURAL);
    PORT (
      a   : IN UNSIGNED(WID - 1 DOWNTO 0);
      b   : IN UNSIGNED(WID - 1 DOWNTO 0);
      c   : IN UNSIGNED(WID - 1 DOWNTO 0);
      sum : OUT UNSIGNED(WID + 1 DOWNTO 0) := (OTHERS => '0'));
  END COMPONENT;
  SIGNAL a_tb   : UNSIGNED(7 DOWNTO 0) := "00000000";
  SIGNAL b_tb   : UNSIGNED(7 DOWNTO 0) := "00000000";
  SIGNAL c_tb   : UNSIGNED(7 DOWNTO 0) := "00000000";
  SIGNAL sum_tb : UNSIGNED(9 DOWNTO 0) := "0000000000";
BEGIN

  shifter : BinaryAdderGenericUnsigned
  GENERIC MAP(WID => 8)
  PORT MAP(
    a   => a_tb,
    b   => b_tb,
    c   => c_tb,
    sum => sum_tb);

  PROCESS
  BEGIN
    a_tb <= "10010110";
    b_tb <= "10000000";
    c_tb <= "00000000";
    WAIT FOR 10 ns;
    a_tb <= "11010110";
    b_tb <= "00100001";
    c_tb <= "10001000";
    WAIT FOR 10 ns;
    a_tb <= "11010110";
    b_tb <= "00000001";
    c_tb <= "10011100";
    WAIT FOR 10 ns;
    -- WAIT FOR 10 ns;
    -- WAIT FOR 10 ns;
    -- WAIT FOR 10 ns;
    -- WAIT FOR 10 ns;
    -- WAIT FOR 10 ns;

    finish;

  END PROCESS;
END ARCHITECTURE;
