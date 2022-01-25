LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY CpuCore IS PORT (
  clk             : IN STD_LOGIC;
  reset           : IN STD_LOGIC;
  enter           : IN STD_LOGIC;
  user_input      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  datapath_output : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
  pc_output       : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
  opcode_output   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
  done            : OUT STD_LOGIC);
END ENTITY;

ARCHITECTURE Structural OF CpuCore IS
  COMPONENT ControlUnit IS PORT (
    clk   : IN STD_LOGIC;
    reset : IN STD_LOGIC;
    enter : IN STD_LOGIC;

    -- Control Signals
    sel_mux        : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    immediate      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    we_accumulator : OUT STD_LOGIC;
    addr_regfile   : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    we_regfile     : OUT STD_LOGIC;
    sel_alu        : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    out_en         : OUT STD_LOGIC;
    flag_zero      : IN STD_LOGIC;
    flag_positive  : IN STD_LOGIC;
    nibble_sel     : OUT STD_LOGIC;
    bit_shift_num  : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);

    -- Progmem Signals
    pm_addr : OUT INTEGER RANGE 0 TO 31 := 0;
    ram_in  : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

    -- Debug Output
    PC_out : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
    OP_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    done   : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT Datapath IS PORT (
    clk   : IN STD_LOGIC;
    reset : IN STD_LOGIC;

    -- Control Signals
    sel_mux        : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    immediate      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    we_accumulator : IN STD_LOGIC;
    addr_regfile   : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    we_regfile     : IN STD_LOGIC;
    sel_alu        : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    out_en         : IN STD_LOGIC;
    nibble_sel     : IN STD_LOGIC;
    bit_shift_num  : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    flag_zero      : OUT STD_LOGIC;
    flag_positive  : OUT STD_LOGIC;

    -- Bus Signals
    user_input      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    datapath_output : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  COMPONENT ProgMem IS PORT (
    pm_addr : IN INTEGER RANGE 0 TO 31 := 0;
    data    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  SIGNAL sel_mux_wire        : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL immediate_wire      : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL we_accumulator_wire : STD_LOGIC;
  SIGNAL addr_regfile_wire   : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL we_regfile_wire     : STD_LOGIC;
  SIGNAL sel_alu_wire        : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL out_en_wire         : STD_LOGIC;
  SIGNAL nibble_sel_wire     : STD_LOGIC;
  SIGNAL bit_shift_num_wire  : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL flag_zero_wire      : STD_LOGIC;
  SIGNAL flag_positive_wire  : STD_LOGIC;

  SIGNAL pm_addr_wire : INTEGER RANGE 0 TO 31;
  SIGNAL ram_wire     : STD_LOGIC_VECTOR(7 DOWNTO 0);
BEGIN

  cu : COMPONENT ControlUnit PORT MAP(
    clk    => clk,
    reset  => reset,
    enter  => enter,
    PC_out => pc_output,
    OP_out => opcode_output,
    done   => done,

    pm_addr => pm_addr_wire,
    ram_in  => ram_wire,

    sel_mux        => sel_mux_wire,
    immediate      => immediate_wire,
    we_accumulator => we_accumulator_wire,
    addr_regfile   => addr_regfile_wire,
    we_regfile     => we_regfile_wire,
    sel_alu        => sel_alu_wire,
    out_en         => out_en_wire,
    nibble_sel     => nibble_sel_wire,
    bit_shift_num  => bit_shift_num_wire,
    flag_zero      => flag_zero_wire,
    flag_positive  => flag_positive_wire);

  dp : COMPONENT Datapath PORT MAP(
    clk             => clk,
    reset           => reset,
    user_input      => user_input,
    datapath_output => datapath_output,

    sel_mux        => sel_mux_wire,
    immediate      => immediate_wire,
    we_accumulator => we_accumulator_wire,
    addr_regfile   => addr_regfile_wire,
    we_regfile     => we_regfile_wire,
    sel_alu        => sel_alu_wire,
    out_en         => out_en_wire,
    nibble_sel     => nibble_sel_wire,
    bit_shift_num  => bit_shift_num_wire,
    flag_zero      => flag_zero_wire,
    flag_positive  => flag_positive_wire);

  ram : COMPONENT ProgMem PORT MAP(
    pm_addr => pm_addr_wire,
    data    => ram_wire);

END ARCHITECTURE;
