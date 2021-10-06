LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY GrayCounter IS
    PORT (
        clk: IN STD_LOGIC;
		  output: OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
END;

ARCHITECTURE Behavioural OF GrayCounter IS
BEGIN
	PROCESS(clk)
	BEGIN
		IF RISING_EDGE(clk) THEN
			CASE output IS
				WHEN "0000" =>
					output <= "0001";
				WHEN "0001" =>
					output <= "0011";
				WHEN "0011" =>
					output <= "0010";
				WHEN "0010" =>
					output <= "0110";
				WHEN "0110" =>
					output <= "0111";
				WHEN "0111" =>
					output <= "0101";
				WHEN "0101" =>
					output <= "0100";
				WHEN "0100" =>
					output <= "1100";
				WHEN "1100" =>
					output <= "1101";
				WHEN "1101" =>
					output <= "1111";
				WHEN "1111" =>
					output <= "1110";
				WHEN "1110" =>
					output <= "1010";
				WHEN "1010" =>
					output <= "1001";
				WHEN "1001" =>
					output <= "1000";
				WHEN "1000" =>
					output <= "0000";
				WHEN others =>
					output <= "0000";
			END CASE;
		END IF;
	END PROCESS;
END ARCHITECTURE;