/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/

#include "student_record.hpp"

bool StudentRecord::operator==(const StudentRecord &sr) const {
    return this->id == sr.id;
}

std::size_t std::hash<StudentRecord>::operator()(StudentRecord const &sr) const noexcept {
    return sr.id;
}