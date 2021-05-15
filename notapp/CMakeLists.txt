add_executable(cmake_testapp main.c observer.c server.c client.c util/util.c util/vector.c util/logger.c util/hashtable.c)

find_package(Curses REQUIRED)

target_link_libraries(cmake_testapp pthread)
target_link_libraries(cmake_testapp ${CURSES_LIBRARIES})