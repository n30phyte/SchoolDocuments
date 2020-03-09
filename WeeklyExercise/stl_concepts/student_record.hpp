/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/

#pragma once

#include <string>

struct StudentRecord {
    std::string name;
    int id, grade;
    /**
     * Comparison operator for use in std::find.
     * @param sr The record to be compared to.
     * @return true if same ID, false if different.
     */
    bool operator==(const StudentRecord& sr) const;
};

namespace std {
    template<>
    struct hash<StudentRecord> {
      /**
       * Hash operator for use with stl containers
       * @param sr record to hash
       * @return hash of record, SR ID in this case.
       */
        std::size_t operator()(StudentRecord const& sr) const noexcept;
    };

}