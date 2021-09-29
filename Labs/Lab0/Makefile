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

build:
	@mkdir -p $(WORK_DIR)

ifeq ($(strip $(TESTBENCH)),)
	@echo "TESTBENCH not set. Use TESTBENCH=<value> to set it."
	@exit 1
endif

	@$(GHDL) -i $(GHDL_FLAGS) --workdir=$(WORK_DIR) --work=work $(TB_DIR)/$(TESTBENCH)_tb$(VHDL_EXT) $(SRC_DIR)
	@$(GHDL) -m $(GHDL_FLAGS) --workdir=$(WORK_DIR) --work=work $(TESTBENCH)_tb

run:
	@$(GHDL) -r $(GHDL_FLAGS) --workdir=$(WORK_DIR) $(TESTBENCH)_tb --vcd=$(SIM_DIR)/$(TESTBENCH)_tb.vcd
