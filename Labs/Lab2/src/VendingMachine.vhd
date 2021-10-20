
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY VendingMachine IS
  PORT (
    clk            : IN STD_LOGIC;
    reset          : IN STD_LOGIC;
    item_sel       : IN STD_LOGIC;                             -- 0 for Soft Drink ($2), 1 for Granola ($4)
    coins_in       : IN STD_LOGIC_VECTOR(1 DOWNTO 0);          -- "00" - 0$, "01" - 1$, "10" - 2$, "11" - 3$
    change_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0) := "00"; -- changeout is displayed on two leds - "00" - 0$ "01" - 1$, "10" - 2$ and "11" - 3$
    display_sum    : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);         -- display the current sum of inserted money on the seven segment
    select_segment : OUT STD_LOGIC := '0';                     -- select the left or right segment
    soft_drink     : OUT STD_LOGIC := '0';                     -- turn on the LED to dispense soft drink
    granola_bar    : OUT STD_LOGIC := '0');                    -- turn on the LED to dispense granola bar

END ENTITY;

ARCHITECTURE Behavioral OF VendingMachine IS
  TYPE state_type IS (sum_0, sum_1, sum_2, sum_3, sum_4, sum_5, sum_6, dispense);
  SIGNAL present_state, next_state : state_type;

  VARIABLE selected_item : STD_LOGIC := '0';

  FUNCTION UNSIGNED_TO_SSD(num : POSITIVE RANGE 0 TO 9)
    RETURN STD_LOGIC_VECTOR IS
  BEGIN
    CASE num IS
      WHEN 0 => RETURN "1111110";
      WHEN 1 => RETURN "0110000";
      WHEN 2 => RETURN "1101101";
      WHEN 3 => RETURN "1111001";
      WHEN 4 => RETURN "0110011";
      WHEN 5 => RETURN "1011011";
      WHEN 6 => RETURN "1011111";
      WHEN 7 => RETURN "1110000";
      WHEN 8 => RETURN "1111111";
      WHEN 9 => RETURN "1110011";
    END CASE;
  END FUNCTION;
BEGIN

  PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      present_state <= sum_0;
    ELSIF rising_edge(clk) THEN
      present_state <= next_state;
    END IF;
  END PROCESS;

  PROCESS (present_state, coins_in)
  BEGIN
    CASE present_state IS
      WHEN sum_0 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(0);

        selected_item := item_sel; -- Capture currently selected item into register

        WITH coins_in SELECT
          next_state <= sum_1 WHEN "01",
          next_state <= sum_2 WHEN "10",
          next_state <= sum_3 WHEN "11",
          next_state <= sum_0 WHEN OTHERS;

      WHEN sum_1 =>
        display_sum <= UNSIGNED_TO_SSD(1);

        WITH coins_in SELECT
          next_state <= sum_2 WHEN "01",
          next_state <= sum_3 WHEN "10",
          next_state <= sum_4 WHEN "11",
          next_state <= sum_0 WHEN OTHERS;

      WHEN sum_2 =>
        display_sum <= UNSIGNED_TO_SSD(2);

        IF selected_item = '0' THEN
          next_state <= dispense;
          change_out <= "00";
        ELSE
          WITH coins_in SELECT
            next_state <= sum_3 WHEN "01",
            next_state <= sum_4 WHEN "10",
            next_state <= sum_5 WHEN "11",
            next_state <= sum_0 WHEN OTHERS;
        END IF;

      WHEN sum_3 =>
        display_sum <= UNSIGNED_TO_SSD(3);

        IF selected_item = '0' THEN
          next_state <= dispense;
          change_out <= "01";
        ELSE
          WITH coins_in SELECT
            next_state <= sum_4 WHEN "01",
            next_state <= sum_5 WHEN "10",
            next_state <= sum_6 WHEN "11",
            next_state <= sum_0 WHEN OTHERS;
        END IF;
      WHEN sum_4 =>
        display_sum <= UNSIGNED_TO_SSD(4);

        IF selected_item = '0' THEN
          next_state <= dispense;
          change_out <= "10";
        ELSIF selected_item = '1' THEN
          next_state <= dispense;
          change_out <= "00";
        END IF;

      WHEN sum_5 =>
        display_sum <= UNSIGNED_TO_SSD(5);

        IF selected_item = '0' THEN
          next_state <= dispense;
          change_out <= "11";
        ELSIF selected_item = '1' THEN
          next_state <= dispense;
          change_out <= "01";
        END IF;
      WHEN sum_6 =>
        display_sum <= UNSIGNED_TO_SSD(6);

        IF selected_item = '1' THEN
          next_state <= dispense;
          change_out <= "10";
        END IF;

      WHEN dispense =>
        display_sum <= "0111101";

        IF selected_item = '0' THEN
          soft_drink <= '1';
        ELSE
          granola_bar <= '1';
        END IF;

        next_state <= sum_0;

    END CASE;
  END PROCESS;
END Behavioral;
