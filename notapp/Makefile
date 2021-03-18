CC = gcc
override CFLAGS += -std=gnu99
BUILD = build

.PHONY: clean format

all: notapp

notapp: $(BUILD)/util.o $(BUILD)/vector.o $(BUILD)/logger.o $(BUILD)/hashtable.o
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) main.c observer.c server.c client.c $(BUILD)/util.o $(BUILD)/vector.o $(BUILD)/logger.o $(BUILD)/hashtable.o -pthread -lncurses -o notapp

notapp.time: $(BUILD)/util.o $(BUILD)/vector.o $(BUILD)/logger.o $(BUILD)/hashtable.o
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -DNOTAPP_TIME main.c observer.c server.c client.c $(BUILD)/util.o $(BUILD)/vector.o $(BUILD)/logger.o $(BUILD)/hashtable.o -pthread -lncurses -o notapp

$(BUILD)/util.o:
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c util/util.c -o $(BUILD)/util.o

$(BUILD)/vector.o:
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c util/vector.c -o $(BUILD)/vector.o

$(BUILD)/logger.o:
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c util/logger.c -o $(BUILD)/logger.o

$(BUILD)/hashtable.o:
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c util/hashtable.c -o $(BUILD)/hashtable.o

format:
	@clang-format -style=file -i *.c *.h
	@clang-format -style=file -i util/*.c util/*.h

clean:
	@rm -rf $(BUILD) notapp
