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
            auto res = table.insert(StudentRecord{name, ID, grade});

            if (!res.second) {
                message << "Error: Cannot insert duplicate ID" << std::endl;
            }
        } else if (input == "R") {
            std::cin >> ID;
            auto target = table.find(StudentRecord{"", ID, 0});

            if (target != table.end()) {
                table.erase(target);
            } else {
                message << "Error: Cannot remove non-existent ID" << std::endl;
            }
        } else if (input == "Q") {
            auto query = StudentRecord();
            std::cin >> input;
            if (input == "i") {
                std::cin >> ID;
                query.id = ID;
            } else if (input == "n") {
                std::cin >> name;
                query.name = name;
            } else if (input == "g") {
                std::cin >> grade;
                query.grade = grade;
            }

            for (const auto &it : table) {
                if (it == query) {
                    message << "Name: " << it.name << ", ID: " << it.id << ", Grade: " << it.grade << std::endl;
                }
            }

            if(message.str().empty()) {
                message << "Error: No matches found" << std::endl;
            }

        } else if (input == "S") {
            stop = true;
        } else {
            message << "Error: Incorrect input." << std::endl;
            std::cin.sync();
        }

        std::cout << message.str();
        message.str(std::string());
    }

    return 0;
}