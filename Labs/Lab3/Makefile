# GHDL settings
GHDL=ghdl
GHDL_FLAGS = --std=08

# Project settings

SRC_DIR=src/*
TB_DIR=test
WORK_DIR=synthesis/ghdl

VHDL_EXT=.vhd

SIM_DIR=simulation

.PHONY: clean

all: clean build run

clean:
	@rm -rf $(WORK_DIR)

datapath:
	@$(GHDL) -i $(GHDL_FLAGS) --workdir=$(WORK_DIR) --work=work datapath/*.vhdl

build:
	@mkdir -p $(WORK_DIR)

	@$(GHDL) -i $(GHDL_FLAGS) --workdir=$(WORK_DIR) --work=work $(TB_DIR)/$(TESTBENCH)_tb$(VHDL_EXT) $(SRC_DIR)
