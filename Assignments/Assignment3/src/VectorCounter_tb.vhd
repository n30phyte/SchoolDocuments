LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY VectorCounter_tb IS
END ENTITY;

ARCHITECTURE Behavioural OF VectorCounter_tb IS
  COMPONENT VectorCounter IS
    PORT (
      CLK           : IN STD_LOGIC;
      RESETn        : IN STD_LOGIC;
      CLEAR_RESULTS : IN STD_LOGIC;
      LOAD          : IN STD_LOGIC;
      SEL_DISPLAY   : IN STD_LOGIC_VECTOR(1 DOWNTO 0);

      vec_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

      ACK   : OUT STD_LOGIC;
      COUNT : OUT UNSIGNED(15 DOWNTO 0);
      ERR   : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL clk_tb           : STD_LOGIC                    := '0';
  SIGNAL resetn_tb        : STD_LOGIC                    := '0';
  SIGNAL clear_results_tb : STD_LOGIC                    := '0';
  SIGNAL load_tb          : STD_LOGIC                    := '0';
  SIGNAL sel_display_tb   : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";

  SIGNAL vec_in_tb : STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');

  SIGNAL ack_tb   : STD_LOGIC;
  SIGNAL count_tb : UNSIGNED(15 DOWNTO 0);
  SIGNAL err_tb   : STD_LOGIC;

  CONSTANT clk_period : TIME := 10 ns;

BEGIN

  dut : COMPONENT VectorCounter
    PORT MAP(
      CLK           => clk_tb,
      RESETn        => resetn_tb,
      CLEAR_RESULTS => clear_results_tb,
      LOAD          => load_tb,
      SEL_DISPLAY   => sel_display_tb,

      vec_in => vec_in_tb,

      ACK   => ack_tb,
      COUNT => count_tb,
      ERR   => err_tb);

    PROCESS
    BEGIN
      clk_tb <= '1';
      WAIT FOR clk_period / 2;
      clk_tb <= '0';
      WAIT FOR clk_period / 2;
    END PROCESS;

    PROCESS
    BEGIN

      WAIT FOR clk_period/2;
      resetn_tb      <= '1';
      vec_in_tb      <= "01110000";
      sel_display_tb <= "01";
      load_tb        <= '1';
      WAIT FOR clk_period/2;
      sel_display_tb <= "10";
      load_tb        <= '0' WHEN ack_tb = '1';
      WAIT FOR clk_period/2;

      std.env.stop;
    END PROCESS;

  END ARCHITECTURE;
