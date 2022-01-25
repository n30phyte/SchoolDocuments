/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/

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

      // Store result to check if insert successful
      auto res = table.insert(StudentRecord{name, ID, grade});

      // unsuccessful insert is duplicate ID
      if (!res.second) {
        message << "Error: Cannot insert duplicate ID" << std::endl;
      }
    } else if (input == "R") {
      std::cin >> ID;
      // Find entry in table
      auto target = table.find(StudentRecord{"", ID, 0});

      if (target != table.end()) {
        // Delete if found
        table.erase(target);
      } else {
        message << "Error: Cannot remove non-existent ID" << std::endl;
      }
    } else if (input == "Q") {
      std::cin >> input;
      if (input == "i") {
        std::cin >> ID;

        // Guaranteed to be unique, just use find.
        auto target = table.find(StudentRecord{"", ID, 0});

        if (target != table.end()) {
          message << "Name: " << target->name << ", ID: " << target->id << ", Grade: " << target->grade << std::endl;
        }

      } else if (input == "n") {
        std::cin >> name;
        // Name and grade not guaranteed unique, use for loop.
        for (const auto &it : table) {
          if (it.name == name) {
            message << "Name: " << it.name << ", ID: " << it.id << ", Grade: " << it.grade << std::endl;
          }
        }
      } else if (input == "g") {
        std::cin >> grade;

        for (const auto &it : table) {
          if (it.grade == grade) {
            message << "Name: " << it.name << ", ID: " << it.id << ", Grade: " << it.grade << std::endl;
          }
        }
      }

      // If no new message from query, no matches.
      if (message.str().empty()) {
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