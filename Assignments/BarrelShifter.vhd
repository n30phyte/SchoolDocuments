-- Code your design here
library IEEE;
use IEEE.std_logic_1164.all;

entity BarrelShifter is
	port (
    	d_in : in STD_LOGIC_VECTOR(7 downto 0);
    	d_out: out STD_LOGIC_VECTOR(7 downto 0);
    	rot: in integer range 0 to 7);
end;

architecture Behavioural of BarrelShifter is
begin
with rot select d_out <=
	d_in when 0,
	d_in(0) & d_in(7 downto 1) when 1,
    d_in(1 downto 0) & d_in(7 downto 2) when 2,
    d_in(2 downto 0) & d_in(7 downto 3) when 3,
    d_in(3 downto 0) & d_in(7 downto 4) when 4,
    d_in(4 downto 0) & d_in(7 downto 5) when 5,
    d_in(5 downto 0) & d_in(7 downto 6) when 6,
    d_in(6 downto 0) & d_in(7) when 7;

end;