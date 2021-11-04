LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY VectorCounterController IS
  PORT (
    CLK           : IN STD_LOGIC;
    RESETn        : IN STD_LOGIC;
    CLEAR_RESULTS : IN STD_LOGIC;
    LOAD          : IN STD_LOGIC;
    SEL_DISPLAY   : IN STD_LOGIC_VECTOR(1 DOWNTO 0);

    ACK   : OUT STD_LOGIC;
    COUNT : OUT UNSIGNED(15 DOWNTO 0);
    ERR   : OUT STD_LOGIC;

    -- Datapath signals
    is_odd_parity   : IN STD_LOGIC;
    has_one_triplet : IN STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioural OF VectorCounterController IS
  SIGNAL total_count      : UNSIGNED(15 DOWNTO 0) := to_unsigned(0, 16);
  SIGNAL odd_parity_count : UNSIGNED(15 DOWNTO 0) := to_unsigned(0, 16);
  SIGNAL group_count      : UNSIGNED(15 DOWNTO 0) := to_unsigned(0, 16);
BEGIN

  WITH SEL_DISPLAY SELECT COUNT <=
    odd_parity_count WHEN "01",
    group_count WHEN "10",
    total_count WHEN OTHERS;

  PROCESS (clk, RESETn)
    VARIABLE ACK_count : INTEGER RANGE 0 TO 2 := 0;
  BEGIN
    IF RESETn = '0' THEN
      ACK              <= '0';
      COUNT            <= to_unsigned(0, 16);
      ERR              <= '0';
      total_count      <= to_unsigned(0, 16);
      odd_parity_count <= to_unsigned(0, 16);
      group_count      <= to_unsigned(0, 16);
    ELSIF rising_edge(clk) THEN
      IF CLEAR_RESULTS = '1' THEN
        ACK              <= '0';
        COUNT            <= to_unsigned(0, 16);
        ERR              <= '0';
        total_count      <= to_unsigned(0, 16);
        odd_parity_count <= to_unsigned(0, 16);
        group_count      <= to_unsigned(0, 16);
      ELSIF ACK_count /= 0 THEN
        IF ACK_count = 1 THEN
          ACK_count := 2;
        ELSE
          ACK_count := 0;
        END IF;
      ELSIF ERR /= '1' AND LOAD = '1' THEN

        ACK <= '1';
        ACK_count := 1;
        total_count <= total_count + 1;

        IF total_count = 0 THEN
          ERR <= '1';
        END IF;

        IF is_odd_parity = '1' THEN
          odd_parity_count <= odd_parity_count + 1;
          IF odd_parity_count = 0 THEN
            ERR <= '1';
          END IF;
        END IF;

        IF has_one_triplet = '1' THEN
          group_count <= group_count + 1;
          IF group_count = 0 THEN
            ERR <= '1';
          END IF;
        END IF;

      END IF;
    END IF;
  END PROCESS;

END ARCHITECTURE;
