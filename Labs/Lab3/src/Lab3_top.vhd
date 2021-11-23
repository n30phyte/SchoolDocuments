LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY Lab3_top IS
  PORT (
    clk            : IN STD_LOGIC;
    rst_button     : IN STD_LOGIC;
    entered_input  : IN STD_LOGIC;
    input_sw       : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    OPcode_LED     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    PC_on_7_seg    : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
    select_segment : OUT STD_LOGIC;
    done_signal    : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Behavioural OF Lab3_top IS

  COMPONENT cpu_ctrl_dp PORT (
    clk_cpu      : IN STD_LOGIC;
    rst_cpu      : IN STD_LOGIC;
    entered_ip   : IN STD_LOGIC;
    input_cpu    : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    output_cpu   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    PC_output    : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
    OPCODE_ouput : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    done_cpu     : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT sev_segment PORT (
    --output of PC from cpu
    DispVal : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
    anode   : OUT STD_LOGIC;
    --controls which digit to display
    segOut : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
  END COMPONENT;

  COMPONENT clk_divider PORT (clk_in : IN STD_LOGIC;
    clk_out                            : OUT STD_LOGIC);
  END COMPONENT;
  SIGNAL clk_1Hz         : STD_LOGIC;
  SIGNAL in_modified     : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL output_from_cpu : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL PC              : STD_LOGIC_VECTOR(4 DOWNTO 0);

BEGIN

  in_modified <= "00000" & input_sw;

  clk_div : clk_divider PORT MAP(
    clk_in  => clk,
    clk_out => clk_1Hz);
  cpu_core : cpu_ctrl_dp PORT MAP(
    clk_cpu      => clk_1Hz,
    rst_cpu      => rst_button,
    entered_ip   => entered_input,
    input_cpu    => in_modified,
    output_cpu   => output_from_cpu,
    PC_output    => PC,
    OPCODE_ouput => OPcode_LED,
    done_cpu     => done_signal);
  seven_seg : COMPONENT sev_segment PORT MAP(
    DispVal => PC,
    anode   => select_segment,
    segOut  => PC_on_7_seg);

END ARCHITECTURE;
