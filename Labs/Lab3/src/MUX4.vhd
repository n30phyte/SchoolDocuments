LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY MUX4 IS PORT (
  sel_mux                            : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
  in3_mux, in2_mux, in1_mux, in0_mux : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  out_mux                            : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF MUX4 IS
BEGIN
  PROCESS (sel_mux, in3_mux, in2_mux, in1_mux, in0_mux)
  BEGIN
    IF (sel_mux = "11") THEN
      out_mux <= in3_mux;
    ELSIF (sel_mux = "10") THEN
      out_mux <= in2_mux;
    ELSIF (sel_mux = "01") THEN
      out_mux <= in1_mux;
    ELSIF (sel_mux = "00") THEN
      out_mux <= in0_mux;
    ELSE
      out_mux <= (OTHERS => 'X');
    END IF;

  END PROCESS;
END ARCHITECTURE;
