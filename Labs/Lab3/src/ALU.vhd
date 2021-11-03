LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ALU IS PORT (
  clk     : IN STD_LOGIC;
  op      : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
  in_a    : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  in_b    : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  rot_num : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
  out_alu : OUT STD_LOGIC_VECTOR (7 DOWNTO 0) := "00000000");
END ENTITY;

ARCHITECTURE Behavioural OF ALU IS
BEGIN
  PROCESS (clk)
  BEGIN
    IF rising_edge(clk) THEN
      CASE op IS
        WHEN "000" =>
          out_alu <= in_a;
        WHEN "001" =>
          out_alu <= in_a AND in_b;
        WHEN "010" =>
          out_alu <= in_a SLL rot_num;
        WHEN "011" =>
          out_alu <= in_a SRL rot_num;
        WHEN "100" =>
          out_alu <= in_a + in_b;
        WHEN "101" =>
          out_alu <= in_a - in_b;
        WHEN "110" =>
          out_alu <= in_a + 1;
        WHEN "111" =>
          out_alu <= in_a - 1;
        WHEN OTHERS =>
          out_alu <= "0000000";
      END CASE;
    END IF;
  END PROCESS;
END ARCHITECTURE;
