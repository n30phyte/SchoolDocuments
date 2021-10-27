--------------------------------------------------------------------------------
-- Vending Machine with Mealy FSM
-- 
--TODO: Describe
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-10-20
-- 
-- Written in VHDL 2008
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY VendingMachine IS
  PORT (
    clk   : IN STD_LOGIC;
    reset : IN STD_LOGIC;

    -- 0 for Soft Drink, 1 for Granola
    item_sel : IN STD_LOGIC;
    -- Represented in binary integers
    coins_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    -- Represented in binary integers
    change_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    -- Seven Segment output of current money in the system
    display_sum : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
    -- Seven Segment select side
    select_segment : OUT STD_LOGIC;
    -- Soft Drink indicator
    soft_drink : OUT STD_LOGIC;
    -- Granola Bar Indicator
    granola_bar : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioral OF VendingMachine IS
  TYPE state_type IS (sum_0, sum_1, sum_2, sum_3, sum_4, sum_5, sum_6, dispense);
  SIGNAL present_state, next_state : state_type;

  FUNCTION UNSIGNED_TO_SSD(num : NATURAL RANGE 0 TO 9)
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

  select_segment <= '0';

  clk_proc : PROCESS (clk, reset)
  BEGIN
    IF rising_edge(clk) THEN
      IF reset = '1' THEN
        present_state <= sum_0;
      ELSE
        present_state <= next_state;
      END IF;
    END IF;
  END PROCESS;

  state_proc : PROCESS (present_state, coins_in, item_sel)
  BEGIN
    CASE present_state IS
      WHEN sum_0 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(0);

        CASE coins_in IS
          WHEN "01"   => next_state   <= sum_1;
          WHEN "10"   => next_state   <= sum_2;
          WHEN "11"   => next_state   <= sum_3;
          WHEN OTHERS => next_state <= sum_0;
        END CASE;

      WHEN sum_1 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(1);

        CASE coins_in IS
          WHEN "01"   => next_state   <= sum_2;
          WHEN "10"   => next_state   <= sum_3;
          WHEN "11"   => next_state   <= sum_4;
          WHEN OTHERS => next_state <= sum_1;
        END CASE;

      WHEN sum_2 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(2);

        IF item_sel = '0' THEN
          change_out <= "00";
          next_state <= dispense;
        ELSE
          CASE coins_in IS
            WHEN "01"   => next_state   <= sum_3;
            WHEN "10"   => next_state   <= sum_4;
            WHEN "11"   => next_state   <= sum_5;
            WHEN OTHERS => next_state <= sum_2;
          END CASE;
        END IF;

      WHEN sum_3 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(3);

        IF item_sel = '0' THEN
          change_out <= "01";
          next_state <= sum_2;
        ELSE
          CASE coins_in IS
            WHEN "01"   => next_state   <= sum_4;
            WHEN "10"   => next_state   <= sum_5;
            WHEN "11"   => next_state   <= sum_6;
            WHEN OTHERS => next_state <= sum_3;
          END CASE;
        END IF;

      WHEN sum_4 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(4);

        IF item_sel = '0' THEN
          change_out <= "10";
          next_state <= sum_2;
        ELSIF item_sel = '1' THEN
          change_out <= "00";
          next_state <= dispense;
        END IF;

      WHEN sum_5 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(5);

        IF item_sel = '0' THEN
          change_out <= "11";
          next_state <= sum_2;
        ELSIF item_sel = '1' THEN
          change_out <= "01";
          next_state <= sum_4;
        END IF;

      WHEN sum_6 =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= UNSIGNED_TO_SSD(6);

        IF item_sel = '0' THEN
          change_out <= "11";
          next_state <= sum_3;
        ELSIF item_sel = '1' THEN
          change_out <= "10";
          next_state <= sum_4;
        END IF;

      WHEN dispense =>
        soft_drink  <= '0';
        granola_bar <= '0';
        change_out  <= "00";

        display_sum <= "0111101";

        IF item_sel = '0' THEN
          soft_drink <= '1';
        ELSE
          granola_bar <= '1';
        END IF;

        next_state <= sum_0;

    END CASE;
  END PROCESS;

END Behavioral;
