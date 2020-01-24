CC = g++
CCFLAGS = -O3 -Wall
LFLAGS = -static

.PHONY: clean

all: flipflop textmatch

flipflop: flipflop.o
	$(CC) -o flipflop flipflop.o $(LFLAGS)

flipflop.o: flipflop.cpp
	$(CC) -c -o flipflop.o flipflop.cpp $(CCFLAGS)

textmatch: textmatch.o
	$(CC) -o textmatch textmatch.o $(LFLAGS)

textmatch.o: textmatch.cpp
	$(CC) -c -o textmatch.o textmatch.cpp $(CCFLAGS)

clean:
	@rm -f *.o textmatch flipflop