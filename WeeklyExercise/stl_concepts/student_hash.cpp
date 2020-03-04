#include <iostream>
#include <unordered_set>
#include <sstream>

#include "student_record.hpp"

int main() {
    std::unordered_set<StudentRecord> table;

    std::string input;
    std::stringstream message;

    bool stop = false;
    while (!stop) {
        std::string name;
        int ID;
        int grade;

        std::cin >> input;

        if (input == "I") {
            std::cin >> name >> ID >> grade;

            table.insert(StudentRecord{name,ID, grade});
        } else if (input == "R") {

        } else if (input == "Q") {

        } else if (input == "S") {
            stop = true;
        } else {
            message << "Error: Incorrect input." << std::endl;
        }

        std::cout << message.str();
        message.clear();
    }

    return 0;
}