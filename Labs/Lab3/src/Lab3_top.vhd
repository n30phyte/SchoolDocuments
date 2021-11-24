LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY Lab3_top IS
  PORT (
    clk            : IN STD_LOGIC;
    reset          : IN STD_LOGIC;
    enter          : IN STD_LOGIC;
    input_sw       : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    OPcode_LED     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    PC_on_7_seg    : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
    select_segment : OUT STD_LOGIC;
    done           : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioural OF Lab3_top IS

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

  COMPONENT SevenSegmentDecoder
    PORT (
      display_value  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
      segment_select : OUT STD_LOGIC;
      segment_output : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
  END COMPONENT;

  COMPONENT ClockDivider PORT (
    clk_in  : IN STD_LOGIC;
    clk_out : OUT STD_LOGIC
    );

  END COMPONENT;
  SIGNAL clk_1Hz         : STD_LOGIC;
  SIGNAL in_modified     : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL output_from_cpu : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL PC              : STD_LOGIC_VECTOR(4 DOWNTO 0);

BEGIN

  in_modified <= "00000" & input_sw;

  clk_div : ClockDivider PORT MAP(
    clk_in  => clk,
    clk_out => clk_1Hz);
  cpu_core : CpuCore PORT MAP(
    clk             => clk_1Hz,
    reset           => reset,
    enter           => enter,
    user_input      => in_modified,
    datapath_output => output_from_cpu,
    pc_output       => PC,
    opcode_output   => OPcode_LED,
    done            => done);
  seven_seg : COMPONENT SevenSegmentDecoder PORT MAP(
    display_value  => PC,
    segment_select => select_segment,
    segment_output => PC_on_7_seg);

END ARCHITECTURE;
