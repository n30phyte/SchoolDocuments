#pragma once

#include <string>

struct StudentRecord {
    std::string name;
    int id, grade;

    bool operator==(const StudentRecord& sr) const;
};

namespace std {
    template<>
    struct hash<StudentRecord> {
        std::size_t operator()(StudentRecord const& sr) const noexcept;
    };

}