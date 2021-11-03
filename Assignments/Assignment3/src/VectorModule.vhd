LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY VectorModule IS
  PORT (
    CLK       : IN STD_LOGIC;
    RESETn    : IN STD_LOGIC;
    vector_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    ack       : OUT STD_LOGIC;
    count     : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    err       : OUT STD_LOGIC
  );
END;

ARCHITECTURE Behavioural OF BinaryAdderGeneric IS
BEGIN

  PROCESS (a, b)

  BEGIN
    adder_array : FOR i IN 0 TO WID - 1 GENERATE

    END GENERATE;
  END PROCESS;
END;
