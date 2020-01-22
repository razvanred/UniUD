cmake_minimum_required(VERSION 3.16)
project(nim
        DESCRIPTION "todo"
        LANGUAGES C)

# warnings
add_library(GNUWarnings INTERFACE)
target_compile_options(GNUWarnings INTERFACE -Wall -Wextra)

# visualt
add_subdirectory(visualt)
target_link_libraries(visualt PRIVATE GNUWarnings)

# header libraries
add_library(threads INTERFACE)
target_link_libraries(threads INTERFACE pthread)

add_library(sockets INTERFACE)

add_library(common INTERFACE)

# server
add_executable(nimServer server.c)
target_compile_features(nimServer PRIVATE c_std_99)
target_link_libraries(nimServer PRIVATE threads sockets common visualt GNUWarnings)

# client
add_executable(nimClient client.c)
target_compile_features(nimClient PRIVATE c_std_99)
target_link_libraries(nimClient PRIVATE sockets common visualt GNUWarnings)


add_executable(tests tests.c)
target_compile_features(tests PRIVATE c_std_99)
#target_link_libraries(tests PRIVATE GNUWarnings)
