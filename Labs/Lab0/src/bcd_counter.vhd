LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY bcd_counter IS
  PORT (
    clk   : IN STD_LOGIC;
    dir   : IN STD_LOGIC;
    reset : IN STD_LOGIC;
    led   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
END ENTITY;

ARCHITECTURE behavioral OF bcd_counter IS
BEGIN

  PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      led <= "0000";
    ELSIF rising_edge(clk) THEN
      IF dir = '1' THEN
        IF led = "1001" THEN
          led <= "0000";
        ELSE
          led <= STD_LOGIC_VECTOR(UNSIGNED(led) + 1);
        END IF;
      ELSE
        IF led = "0000" THEN
          led <= "1001";
        ELSE
          led <= STD_LOGIC_VECTOR(UNSIGNED(led) - 1);
        END IF;
      END IF;
    END IF;
  END PROCESS;

END ARCHITECTURE;
