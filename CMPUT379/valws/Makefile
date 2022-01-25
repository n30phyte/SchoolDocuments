CC = gcc
override CFLAGS += -std=c99 -O3
OBJ = obj

.PHONY: clean

valws379: $(OBJ)/valws379.o $(OBJ)/queue.o $(OBJ)/lru.o $(OBJ)/multiset.o
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) $(OBJ)/valws379.o $(OBJ)/queue.o $(OBJ)/lru.o $(OBJ)/multiset.o -o valws379

$(OBJ)/queue.o: util/queue.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c queue.c -o $(OBJ)/queue.o
$(OBJ)/lru.o: util/lru.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c lru.c -o $(OBJ)/lru.o
$(OBJ)/multiset.o: util/multiset.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c multiset.c -o $(OBJ)/multiset.o
$(OBJ)/valws379.o: main.c
	@mkdir -p $(OBJ)
	$(CC) $(CFLAGS) -c main.c -o $(OBJ)/valws379.o

clean:
	@rm -rf $(OBJ) valws379
