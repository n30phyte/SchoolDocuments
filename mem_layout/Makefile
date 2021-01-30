CC = gcc
override CFLAGS += -std=c99 -m32
OBJ = obj

.PHONY: clean

all: mem_1 mem_2 mem_3

mem_1: $(OBJ)/mem_1.o $(OBJ)/memlayout.o
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) $(OBJ)/mem_1.o $(OBJ)/memlayout.o -o mem_1
mem_2: $(OBJ)/mem_2.o $(OBJ)/memlayout.o
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) $(OBJ)/mem_2.o $(OBJ)/memlayout.o -o mem_2
mem_3: $(OBJ)/mem_3.o $(OBJ)/memlayout.o
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) $(OBJ)/mem_3.o $(OBJ)/memlayout.o -o mem_3

$(OBJ)/mem_1.o: mem_1.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c mem_1.c -o $(OBJ)/mem_1.o
$(OBJ)/mem_2.o: mem_2.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c mem_2.c -o $(OBJ)/mem_2.o
$(OBJ)/mem_3.o: mem_3.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c mem_3.c -o $(OBJ)/mem_3.o
$(OBJ)/memlayout.o: memlayout.c memlayout.h
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -Wno-int-to-pointer-cast -c memlayout.c -o $(OBJ)/memlayout.o

clean:
	@rm -rf $(OBJ) mem_1 mem_2 mem_3
