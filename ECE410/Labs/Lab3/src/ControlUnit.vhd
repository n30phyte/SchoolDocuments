LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ControlUnit IS PORT (
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
END ENTITY;

ARCHITECTURE Behavioural OF ControlUnit IS

  TYPE cycle_state_t IS (FETCH, DECODE, EXECUTE, STOP_CPU);

  TYPE instruction_t IS (OP_LDA, OP_STA, OP_LDI, OP_ALU, OP_INA, OP_OUTA, OP_HALT, OP_JZ);

  SIGNAL controller_state : cycle_state_t;
  SIGNAL IR               : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL PC               : INTEGER RANGE 0 TO 31 := 0;

  CONSTANT LDA : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0001";
  CONSTANT STA : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0010";
  CONSTANT LDI : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0011";

  CONSTANT ADD : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0100";
  CONSTANT SUB : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0101";

  CONSTANT SHFL : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0110";
  CONSTANT SHFR : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0111";

  CONSTANT INA  : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1000";
  CONSTANT OUTA : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1001";
  CONSTANT HALT : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1010";

  CONSTANT JZ : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1100";

BEGIN

  OP_out  <= IR(7 DOWNTO 4);                       -- Hardwire opcode output to current instruction register
  PC_out  <= STD_LOGIC_VECTOR(to_unsigned(PC, 5)); -- Hardwire PC output to current PC
  pm_addr <= PC;

  PROCESS (clk, controller_state)
    VARIABLE OPCODE     : STD_LOGIC_VECTOR(3 DOWNTO 0);
    VARIABLE OP_DECODED : instruction_t;
  BEGIN

    IF rising_edge(clk) THEN
      IF reset = '1' THEN -- RESET initializes all the control signals to 0.

        PC               <= 0;
        sel_mux          <= "00";
        immediate        <= (OTHERS => '0');
        we_accumulator   <= '0';
        addr_regfile     <= "000";
        we_regfile       <= '0';
        sel_alu          <= "000";
        out_en           <= '0';
        done             <= '0';
        nibble_sel       <= '0';
        bit_shift_num    <= "00";
        controller_state <= Fetch;

      ELSE

        CASE controller_state IS

          WHEN Fetch => -- fetch instruction
            IF enter = '1' THEN
              IR               <= ram_in;          -- Load current instruction into register
              PC               <= PC + 1;          -- Increment PC
              sel_mux          <= "00";            -- Reset mux selection
              immediate        <= (OTHERS => '0'); -- Reset immediate input
              we_accumulator   <= '0';             -- No write enable for accumulator
              addr_regfile     <= "000";           -- No address for reg file
              we_regfile       <= '0';             -- Disable regfile write
              sel_alu          <= "000";           -- Deselect ALU
              out_en           <= '0';             -- Disable outut
              done             <= '0';             -- Set not done
              controller_state <= Decode;          -- Decode instruction
            END IF;

          WHEN Decode =>

            OPCODE := IR(7 DOWNTO 4);

            controller_state <= EXECUTE;

            CASE OPCODE IS
              WHEN LDA =>
                OP_DECODED := OP_LDA;
              WHEN STA =>
                OP_DECODED := OP_STA;
              WHEN LDI =>
                OP_DECODED := OP_LDI;
              WHEN ADD | SUB | SHFL | SHFR =>
                OP_DECODED := OP_ALU;
              WHEN INA =>
                OP_DECODED := OP_INA;
              WHEN OUTA =>
                OP_DECODED := OP_OUTA;
                controller_state <= EXECUTE;
              WHEN JZ =>
                OP_DECODED := OP_JZ;
                controller_state <= EXECUTE;
              WHEN OTHERS =>
                controller_state <= STOP_CPU;
            END CASE;

            sel_mux        <= "00";
            immediate      <= ram_in; -- Prefetch next instruction as immediate
            we_accumulator <= '0';
            addr_regfile   <= IR(2 DOWNTO 0); -- Prefetch register file
            we_regfile     <= '0';
            sel_alu        <= "000";
            out_en         <= '0';
            done           <= '0';
            nibble_sel     <= IR(0);
            bit_shift_num  <= IR(1 DOWNTO 0);

          WHEN EXECUTE =>

            CASE OP_DECODED IS
              WHEN OP_ALU =>
                sel_mux          <= "00";
                immediate        <= (OTHERS => '0');
                we_accumulator   <= '1';
                addr_regfile     <= IR(2 DOWNTO 0);
                we_regfile       <= '0';
                sel_alu          <= IR(6 DOWNTO 4);
                out_en           <= '0';
                controller_state <= Fetch;

              WHEN OP_LDA =>
                sel_mux          <= "01";
                immediate        <= (OTHERS => '0');
                we_accumulator   <= '1';
                addr_regfile     <= "000";
                we_regfile       <= '0';
                sel_alu          <= "000";
                out_en           <= '0';
                controller_state <= Fetch;

              WHEN OP_STA =>
                sel_mux          <= "00";
                immediate        <= (OTHERS => '0');
                we_accumulator   <= '0';
                addr_regfile     <= IR(2 DOWNTO 0);
                we_regfile       <= '1';
                sel_alu          <= "000";
                out_en           <= '0';
                done             <= '0';
                controller_state <= Fetch;

              WHEN OP_LDI =>
                sel_mux          <= "11";
                immediate        <= ram_in;
                we_accumulator   <= '1';
                addr_regfile     <= "000";
                we_regfile       <= '0';
                sel_alu          <= "000";
                out_en           <= '0';
                done             <= '0';
                PC               <= PC + 1;
                controller_state <= Fetch;

              WHEN OP_JZ =>
                sel_mux        <= "11";
                immediate      <= (OTHERS => '0');
                we_accumulator <= '0';
                addr_regfile   <= "000";
                we_regfile     <= '0';
                sel_alu        <= "000";
                out_en         <= '0';
                done           <= '0';
                IF flag_zero = '1' THEN
                  PC <= to_integer(unsigned(ram_in(4 DOWNTO 0)));
                ELSE
                  PC <= PC + 1;
                END IF;
                controller_state <= Fetch;

              WHEN OP_INA =>
                sel_mux          <= "10";
                immediate        <= (OTHERS => '0');
                we_accumulator   <= '1';
                addr_regfile     <= "000";
                we_regfile       <= '0';
                sel_alu          <= "000";
                out_en           <= '0';
                done             <= '0';
                nibble_sel       <= IR(0);
                controller_state <= Fetch;

              WHEN OP_OUTA =>
                sel_mux          <= "00";
                immediate        <= (OTHERS => '0');
                we_accumulator   <= '0';
                addr_regfile     <= "000";
                we_regfile       <= '0';
                sel_alu          <= "000";
                out_en           <= '1';
                controller_state <= Fetch;

              WHEN OP_HALT =>
                controller_state <= STOP_CPU;
            END CASE;

          WHEN STOP_CPU =>
            sel_mux        <= "00";
            immediate      <= (OTHERS => '0');
            we_accumulator <= '0';
            addr_regfile   <= "000";
            we_regfile     <= '0';
            sel_alu        <= "000";
            out_en         <= '0';
            done           <= '1';
          WHEN OTHERS =>
            controller_state <= STOP_CPU;
        END CASE;
      END IF;
    END IF;
  END PROCESS;
END ARCHITECTURE;
