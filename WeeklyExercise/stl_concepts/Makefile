#################################
#  Name: Michael Kwok
#  ID: 1548454
#  CMPUT 275, Winter 2020
#  Weekly Exercise 6
#################################

CC = g++
CCFLAGS = -O2 -std=c++11
LFLAGS = -static

.PHONY: clean

all: student calc

student: student_hash.o student_record.o
	$(CC) -o student student_hash.o student_record.o $(LFLAGS)

student_hash.o: student_hash.cpp 
	$(CC) -c -o student_hash.o student_hash.cpp $(CCFLAGS) 

student_record.o: student_record.cpp student_record.hpp
	$(CC) -c -o student_record.o student_record.cpp $(CCFLAGS) 

calc: complex_postfix.o complex.o
	$(CC) -o calc complex_postfix.o complex.o $(LFLAGS)

complex_postfix.o: complex_postfix.cpp
	$(CC) -c complex_postfix.cpp $(CCFLAGS) -o complex_postfix.o

complex.o: complex.cpp complex.hpp
	$(CC) -c complex.cpp $(CCFLAGS) -o complex.o

clean:
	@rm -f *.o student calc
