#include "student_record.hpp"

bool StudentRecord::operator==(const StudentRecord &sr) const {
    return (this->id == sr.id) || (this->name == sr.name) || (this->grade == sr.grade);
}

std::size_t std::hash<StudentRecord>::operator()(StudentRecord const &sr) const noexcept {
    return sr.id;
}