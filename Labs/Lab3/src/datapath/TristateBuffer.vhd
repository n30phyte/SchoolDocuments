LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY TristateBuffer IS PORT (
  enable : IN STD_LOGIC;
  D      : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
  Y      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY;

ARCHITECTURE Behavioural OF TriStateBuffer IS
BEGIN
  PROCESS
  BEGIN
    IF enable = '1' THEN
      Y <= D;
    ELSE
      Y <= (OTHERS => 'Z');
    END IF;
  END PROCESS;
END ARCHITECTURE;
