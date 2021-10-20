## Clock signal
set_property -dict {PACKAGE_PIN K17 IOSTANDARD LVCMOS33} [get_ports { global_clk }];
create_clock -period 8.000 -name sys_clk_pin -waveform {0.000 4.000} -add [get_ports { global_clk }];

## Switches
set_property -dict { PACKAGE_PIN G15   IOSTANDARD LVCMOS33 } [get_ports { item_sel }]; #IO_L19N_T3_VREF_35 Sch=sw[0]
# set_property -dict { PACKAGE_PIN P15   IOSTANDARD LVCMOS33 } [get_ports { sw[1] }]; #IO_L24P_T3_34 Sch=sw[1]
# set_property -dict { PACKAGE_PIN W13   IOSTANDARD LVCMOS33 } [get_ports { sw[2] }]; #IO_L4N_T0_34 Sch=sw[2]
# set_property -dict { PACKAGE_PIN T16   IOSTANDARD LVCMOS33 } [get_ports { sw[3] }]; #IO_L9P_T1_DQS_34 Sch=sw[3]

## Buttons
set_property -dict { PACKAGE_PIN K18 IOSTANDARD LVCMOS33 } [get_ports { reset }]; #IO_L12N_T1_MRCC_35 Sch=btn[0]
# set_property -dict { PACKAGE_PIN P16 IOSTANDARD LVCMOS33 } [get_ports { item_sel }]; #IO_L24N_T3_34 Sch=btn[1]
set_property -dict { PACKAGE_PIN K19 IOSTANDARD LVCMOS33 } [get_ports { coins_in[1] }]; #IO_L10P_T1_AD11P_35 Sch=btn[2]
set_property -dict { PACKAGE_PIN Y16 IOSTANDARD LVCMOS33 } [get_ports { coins_in[0] }]; #IO_L7P_T1_34 Sch=btn[3]


## LEDs
# set_property -dict { PACKAGE_PIN M14 IOSTANDARD LVCMOS33 } [get_ports { soft_drink }]; #IO_L23P_T3_35 Sch=led[0]
# set_property -dict { PACKAGE_PIN M15 IOSTANDARD LVCMOS33 } [get_ports { granola_bar }]; #IO_L23N_T3_35 Sch=led[1]
set_property -dict { PACKAGE_PIN G14 IOSTANDARD LVCMOS33 } [get_ports { change_out[1] }]; #IO_0_35 Sch=led[2]
set_property -dict { PACKAGE_PIN D18 IOSTANDARD LVCMOS33 } [get_ports { change_out[0] }]; #IO_L3N_T0_DQS_AD1N_35 Sch=led[3]

## RGB LED 6
set_property -dict { PACKAGE_PIN V16   IOSTANDARD LVCMOS33 } [get_ports { soft_drink }]; #IO_L18P_T2_34 Sch=led6_r
set_property -dict { PACKAGE_PIN F17   IOSTANDARD LVCMOS33 } [get_ports { granola_bar }]; #IO_L6N_T0_VREF_35 Sch=led6_g
# set_property -dict { PACKAGE_PIN M17   IOSTANDARD LVCMOS33 } [get_ports { led6_b }]; #IO_L8P_T1_AD10P_35 Sch=led6_b

## Pmod Header JC
set_property -dict { PACKAGE_PIN V15   IOSTANDARD LVCMOS33     } [get_ports { display_sum[6] }]; #IO_L10P_T1_34 Sch=jc_p[1] AA
set_property -dict { PACKAGE_PIN W15   IOSTANDARD LVCMOS33     } [get_ports { display_sum[5] }]; #IO_L10N_T1_34 Sch=jc_n[1] AB
set_property -dict { PACKAGE_PIN T11   IOSTANDARD LVCMOS33     } [get_ports { display_sum[4] }]; #IO_L1P_T0_34 Sch=jc_p[2] AC
set_property -dict { PACKAGE_PIN T10   IOSTANDARD LVCMOS33     } [get_ports { display_sum[3] }]; #IO_L1N_T0_34 Sch=jc_n[2] AD
#set_property -dict { PACKAGE_PIN W14   IOSTANDARD LVCMOS33     } [get_ports { jc[4] }]; #IO_L8P_T1_34 Sch=jc_p[3]
#set_property -dict { PACKAGE_PIN Y14   IOSTANDARD LVCMOS33     } [get_ports { jc[5] }]; #IO_L8N_T1_34 Sch=jc_n[3]
#set_property -dict { PACKAGE_PIN T12   IOSTANDARD LVCMOS33     } [get_ports { jc[6] }]; #IO_L2P_T0_34 Sch=jc_p[4]
#set_property -dict { PACKAGE_PIN U12   IOSTANDARD LVCMOS33     } [get_ports { jc[7] }]; #IO_L2N_T0_34 Sch=jc_n[4]


## Pmod Header JD
set_property -dict { PACKAGE_PIN T14   IOSTANDARD LVCMOS33     } [get_ports { display_sum[2] }]; #IO_L5P_T0_34 Sch=jd_p[1] AE
set_property -dict { PACKAGE_PIN T15   IOSTANDARD LVCMOS33     } [get_ports { display_sum[1] }]; #IO_L5N_T0_34 Sch=jd_n[1] AF
set_property -dict { PACKAGE_PIN P14   IOSTANDARD LVCMOS33     } [get_ports { display_sum[0] }]; #IO_L6P_T0_34 Sch=jd_p[2] AG
set_property -dict { PACKAGE_PIN R14   IOSTANDARD LVCMOS33     } [get_ports { select_segment }]; #IO_L6N_T0_VREF_34 Sch=jd_n[2]
#set_property -dict { PACKAGE_PIN U14   IOSTANDARD LVCMOS33     } [get_ports { jd[4] }]; #IO_L11P_T1_SRCC_34 Sch=jd_p[3]
#set_property -dict { PACKAGE_PIN U15   IOSTANDARD LVCMOS33     } [get_ports { jd[5] }]; #IO_L11N_T1_SRCC_34 Sch=jd_n[3]
#set_property -dict { PACKAGE_PIN V17   IOSTANDARD LVCMOS33     } [get_ports { jd[6] }]; #IO_L21P_T3_DQS_34 Sch=jd_p[4]
#set_property -dict { PACKAGE_PIN V18   IOSTANDARD LVCMOS33     } [get_ports { jd[7] }]; #IO_L21N_T3_DQS_34 Sch=jd_n[4]


