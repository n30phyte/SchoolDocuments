CC = g++
CCFLAGS = -O2 -std=c++11
LFLAGS = -static

SRCS = graph_concepts.cpp digraph.cpp
OBJS = graph_concepts.o digraph.o

.PHONY: clean

graph_concepts: $(OBJS)
	$(CC) -o graph_concepts $(OBJS) $(LFLAGS)

graph_concepts.o: graph_concepts.cpp
	$(CC) -c -o graph_concepts.o graph_concepts.cpp $(CCFLAGS)

digraph.o: digraph.cpp digraph.h
	$(CC) -c -o digraph.o digraph.cpp $(CCFLAGS)

clean:
	@rm -f $(OBJS) graph_concepts
