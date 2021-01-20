CC = gcc
override CFLAGS += -std=c99 -m32
OBJ = obj

.PHONY: clean directories

all: mem_1 mem_2 mem_3

mem_1: mem_1.o memlayout.o
	$(CC) $(CFLAGS) $(OBJ)/mem_1.o $(OBJ)/memlayout.o -o mem_1
mem_2: mem_2.o memlayout.o
	$(CC) $(CFLAGS) $(OBJ)/mem_2.o $(OBJ)/memlayout.o -o mem_2
mem_3: mem_3.o memlayout.o
	$(CC) $(CFLAGS) $(OBJ)/mem_3.o $(OBJ)/memlayout.o -o mem_3

mem_1.o: mem_1.c directories
	$(CC) $(CFLAGS) -c mem_1.c -o $(OBJ)/mem_1.o
mem_2.o: mem_2.c directories
	$(CC) $(CFLAGS) -c mem_2.c -o $(OBJ)/mem_2.o
mem_3.o: mem_3.c directories
	$(CC) $(CFLAGS) -c mem_3.c -o $(OBJ)/mem_3.o
memlayout.o: memlayout.c memlayout.h directories
	$(CC) $(CFLAGS) -c memlayout.c -o $(OBJ)/memlayout.o

directories:
	@mkdir -p $(OBJ)

clean:
	@rm -rf $(OBJ) mem_1 mem_2 mem_3
