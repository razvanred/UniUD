#=============================
# Preamble
#=============================
cmake_minimum_required(VERSION 3.17)

#=============================
# Project wide setup
#=============================
project(ASD-progetto
        VERSION 1.0.0
        DESCRIPTION "progetto ASD 2021/22"
        HOMEPAGE_URL "https://github.com/razvnred/UniUD/tree/master/Cavasin/laboratorio%20algoritmi/progetto"
        LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_FOLDER progetto)

#=============================
# Targets Configuration
#=============================
if(MSVC)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS _CRT_NONSTDC_NO_WARNINGS)
endif()
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    add_compile_options("$<$<CONFIG:Debug>:-Wall;-Wextra;-Wpedantic;-Wconversion>")
endif()

#=============================
# Targets
#=============================
add_library(select OBJECT
            select.c
            types.h
            heap.c)

add_executable(standalone
               standalone.c
               "${ASD-LAB_SOURCE_DIR}/utils/utils.c")
target_link_libraries(standalone
                      PRIVATE
                      select)

add_executable(driver
               driver.c
               timespec_math.c
               "${ASD-LAB_SOURCE_DIR}/utils/utils.c")
target_link_libraries(driver
                      PRIVATE
                      select m)
target_compile_definitions(driver
                           PRIVATE
                           "$<$<CONFIG:Debug>:DEBUG>")
