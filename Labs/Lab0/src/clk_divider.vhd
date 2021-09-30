LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY clk_divider IS
  PORT (
    clk_in  : IN STD_LOGIC;
    clk_out : OUT STD_LOGIC
  );
END ENTITY;

ARCHITECTURE behavioral OF clk_divider IS
  SIGNAL count : INTEGER := 1;
BEGIN

  PROCESS (clk_in)
  BEGIN
    IF rising_edge(clk_in) THEN
      count <= count + 1;
      IF count = 62500000 THEN
        clk_out <= NOT clk_out;
        count   <= 1;
      END IF;
    END IF;
  END PROCESS;

END ARCHITECTURE;
