add_executable(notapp main.c observer.c server.c client.c util/util.c util/vector.c util/logger.c util/hashtable.c)

find_package(Curses REQUIRED)

target_link_libraries(notapp pthread)
target_link_libraries(notapp ${CURSES_LIBRARIES})