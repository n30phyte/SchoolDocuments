## Clock signal
set_property -dict {PACKAGE_PIN K17 IOSTANDARD LVCMOS33} [get_ports { clk }];
create_clock -period 8.000 -name sys_clk_pin -waveform {0.000 4.000} -add [get_ports { clk }];


## Buttons
set_property -dict { PACKAGE_PIN K19 IOSTANDARD LVCMOS33 } [get_ports { coins_in[0] }]; #IO_L10P_T1_AD11P_35 Sch=btn[2]
set_property -dict { PACKAGE_PIN Y16 IOSTANDARD LVCMOS33 } [get_ports { coins_in[1] }]; #IO_L7P_T1_34 Sch=btn[3]


## LEDs
set_property -dict {PACKAGE_PIN G14 IOSTANDARD LVCMOS33} [get_ports { change_out[0] }]; #IO_0_35 Sch=led[2]
set_property -dict {PACKAGE_PIN D18 IOSTANDARD LVCMOS33} [get_ports { change_out[1] }]; #IO_L3N_T0_DQS_AD1N_35 Sch=led[3]

## Pmod Header JD
set_property -dict { PACKAGE_PIN T14 IOSTANDARD LVCMOS33 } [get_ports { display_sum[0] }]; #IO_L5P_T0_34 Sch=jd_p[1] A
set_property -dict { PACKAGE_PIN P14 IOSTANDARD LVCMOS33 } [get_ports { display_sum[1] }]; #IO_L6P_T0_34 Sch=jd_p[2] B
set_property -dict { PACKAGE_PIN U14 IOSTANDARD LVCMOS33 } [get_ports { display_sum[2] }]; #IO_L11P_T1_SRCC_34 Sch=jd_p[3] C
set_property -dict { PACKAGE_PIN V17 IOSTANDARD LVCMOS33 } [get_ports { display_sum[3] }]; #IO_L21P_T3_DQS_34 Sch=jd_p[4] D
set_property -dict { PACKAGE_PIN T15 IOSTANDARD LVCMOS33 } [get_ports { display_sum[4] }]; #IO_L5N_T0_34 Sch=jd_n[1] E
set_property -dict { PACKAGE_PIN R14 IOSTANDARD LVCMOS33 } [get_ports { display_sum[5] }]; #IO_L6N_T0_VREF_34 Sch=jd_n[2] F
set_property -dict { PACKAGE_PIN U15 IOSTANDARD LVCMOS33 } [get_ports { display_sum[6] }]; #IO_L11N_T1_SRCC_34 Sch=jd_n[3] G
set_property -dict { PACKAGE_PIN V18 IOSTANDARD LVCMOS33 } [get_ports { select_segment }]; #IO_L21N_T3_DQS_34 Sch=jd_n[4] SELECT
