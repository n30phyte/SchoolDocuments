--------------------------------------------------------------------------------
-- CPU Accumulator
-- 
-- Authors: Michael Kwok (mkwok1@ualberta.ca)
-- Create Date: 2021-11-02
-- 
-- Written in VHDL 2008
-- Entity Description:
--   clk            : IN STD_LOGIC;
--  reset          : IN STD_LOGIC;
--  sel_mux        : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
--  immediate      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
--  user_input     : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
--  we_accumulator : IN STD_LOGIC;
--  addr_regfile       : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
--  we_regfile         : IN STD_LOGIC;
--  sel_alu        : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
--  out_en         : IN STD_LOGIC;
--  bit_sel        : IN STD_LOGIC;
--  bit_shift_num : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
--  flag_zero : OUT STD_LOGIC;
--  flag_positive : OUT STD_LOGIC;
--  datapath_output  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.MATH_REAL.ALL;

ENTITY Datapath IS PORT (
  clk             : IN STD_LOGIC;
  reset           : IN STD_LOGIC;
  sel_mux         : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
  immediate       : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  user_input      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  we_accumulator  : IN STD_LOGIC;
  addr_regfile    : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
  we_regfile      : IN STD_LOGIC;
  sel_alu         : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
  out_en          : IN STD_LOGIC;
  nibble_sel      : IN STD_LOGIC;
  bit_shift_num   : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
  flag_zero       : OUT STD_LOGIC;
  flag_positive   : OUT STD_LOGIC;
  datapath_output : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Structural OF Datapath IS

  COMPONENT MUX4 IS PORT (
    sel        : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    A, B, C, D : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    Y          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  COMPONENT Accumulator IS PORT (
    clk   : IN STD_LOGIC;
    rst   : IN STD_LOGIC;
    we    : IN STD_LOGIC;
    D_in  : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    D_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0));
  END COMPONENT;

  COMPONENT TristateBuffer IS PORT (
    enable : IN STD_LOGIC;
    D      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    Y      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  COMPONENT RegisterFile IS
    GENERIC (
      WID : POSITIVE);
    PORT (
      clk   : IN STD_LOGIC;
      we    : IN STD_LOGIC;
      addr  : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
      D_in  : IN STD_LOGIC_VECTOR(WID - 1 DOWNTO 0);
      D_out : OUT STD_LOGIC_VECTOR(WID - 1 DOWNTO 0));
  END COMPONENT;

  COMPONENT ALU IS PORT (
    op      : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    A       : IN SIGNED(7 DOWNTO 0);
    B       : IN SIGNED(7 DOWNTO 0);
    rot_num : IN INTEGER RANGE 0 TO 3;
    Y       : OUT SIGNED (7 DOWNTO 0));
  END COMPONENT;

  SIGNAL shifted_user_input : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL mux_output         : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL accumulator_output : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL regfile_output     : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL alu_output         : SIGNED(7 DOWNTO 0);
BEGIN

  mux : COMPONENT MUX4 PORT MAP(
    sel => sel_mux,
    D   => immediate,
    C   => shifted_user_input,
    B   => regfile_output,
    A   => STD_LOGIC_VECTOR(alu_output),
    Y   => mux_output);

  accu : COMPONENT Accumulator PORT MAP(
    clk   => clk,
    rst   => reset,
    we    => we_accumulator,
    D_in  => mux_output,
    D_out => accumulator_output);

  regfile : COMPONENT RegisterFile GENERIC MAP(
    WID => 8)
  PORT MAP(
    clk   => clk,
    we    => we_regfile,
    addr  => addr_regfile,
    D_in  => accumulator_output,
    D_out => regfile_output);

  arith : COMPONENT ALU PORT MAP(
    op      => sel_alu,
    A       => SIGNED(accumulator_output),
    B       => SIGNED(regfile_output),
    rot_num => TO_INTEGER(UNSIGNED(bit_shift_num)),
    Y       => alu_output);

  tristate_buffer : COMPONENT TristateBuffer PORT MAP(
    enable => out_en,
    D      => accumulator_output,
    Y      => datapath_output
  );

  PROCESS (nibble_sel)
  BEGIN

    IF nibble_sel = '1' THEN
      shifted_user_input <= user_input SLL 4;
      ELSE
      shifted_user_input <= user_input;
    END IF;

  END PROCESS;

  PROCESS (clk)
  BEGIN
    IF rising_edge(clk) THEN
      flag_zero     <= NOR mux_output;
      flag_positive <= NOT mux_output(7);
    END IF;
  END PROCESS;

END ARCHITECTURE;
