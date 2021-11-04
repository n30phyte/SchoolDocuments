LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY VectorCounter IS
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
END ENTITY;

ARCHITECTURE Structural OF VectorCounter IS
  COMPONENT VectorCounterController IS
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
  END COMPONENT;

  COMPONENT VectorCounterDatapath IS
    PORT (
      A               : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      is_odd_parity   : OUT STD_LOGIC;
      has_one_triplet : OUT STD_LOGIC);
  END COMPONENT;

  SIGNAL odd_parity_signal : STD_LOGIC;
  SIGNAL triplet_signal    : STD_LOGIC;

BEGIN

  controller : COMPONENT VectorCounterController
    PORT MAP(
      CLK           => CLK,
      RESETn        => RESETn,
      CLEAR_RESULTS => CLEAR_RESULTS,
      LOAD          => LOAD,
      SEL_DISPLAY   => SEL_DISPLAY,
      ACK           => ACK,
      COUNT         => COUNT,
      ERR           => ERR,

      is_odd_parity   => odd_parity_signal,
      has_one_triplet => triplet_signal);

    datapath : COMPONENT VectorCounterDatapath
      PORT MAP(
        A               => vec_in,
        is_odd_parity   => odd_parity_signal,
        has_one_triplet => triplet_signal);

    END ARCHITECTURE;
