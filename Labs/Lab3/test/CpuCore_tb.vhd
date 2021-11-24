LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY CpuCore_tb IS
END ENTITY;

ARCHITECTURE Behavioural OF CpuCore_tb IS
  COMPONENT CpuCore IS PORT (
    clk             : IN STD_LOGIC;
    reset           : IN STD_LOGIC;
    enter           : IN STD_LOGIC;
    user_input      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    datapath_output : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    pc_output       : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
    opcode_output   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    done            : OUT STD_LOGIC);
  END COMPONENT;

  -- Component Declaration for the Unit Under Test (UUT)

  SIGNAL clk_tb    : STD_LOGIC := '0';
  SIGNAL rst_tb    : STD_LOGIC := '0';
  SIGNAL in_tb     : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL opcode_tb : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL pc_tb     : STD_LOGIC_VECTOR(4 DOWNTO 0);
  SIGNAL output_tb : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL enter     : STD_LOGIC;
  SIGNAL done      : STD_LOGIC;

  -- Clock period definitions
  CONSTANT clk_period : TIME := 8 ns;

BEGIN
  -- Instantiate the Unit Under Test (UUT)
  uut : CpuCore PORT MAP(
    clk             => clk_tb,
    reset           => rst_tb,
    enter           => enter,
    user_input      => in_tb,
    datapath_output => output_tb,
    pc_output       => pc_tb,
    opcode_output   => opcode_tb,
    done            => done);

  clk_process : PROCESS
  BEGIN
    IF done /= '1' THEN
      clk_tb <= '0';
      WAIT FOR clk_period/2;
      clk_tb <= '1';
      WAIT FOR clk_period/2;
    ELSE
      std.env.stop;
    END IF;
  END PROCESS;

  -- Stimulus process
  stim_proc : PROCESS
  BEGIN
    rst_tb <= '1';
    enter  <= '1';
    in_tb  <= "00001101";
    WAIT FOR clk_period;
    rst_tb <= '0';
    enter  <= '1';
    WAIT UNTIL done = '1';
  END PROCESS;
END ARCHITECTURE;
