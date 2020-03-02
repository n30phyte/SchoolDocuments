CC = g++
CCFLAGS = -O3 -std=c++11
LFLAGS = -static

SRCS = exercise4.cpp hash_table.h linked_list.h
OBJS = exercise4.o

.PHONY: clean

exercise4: $(OBJS)
	$(CC) -o exercise4 $(OBJS) $(LFLAGS)
exercise4.o: $(SRCS)
	$(CC) -c -o exercise4.o exercise4.cpp $(CCFLAGS)

clean:
	@rm -f *.o exercise4
