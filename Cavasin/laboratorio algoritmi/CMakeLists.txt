#=============================
# Preamble
#=============================
cmake_minimum_required(VERSION 3.17)

get_property(isMultiConfig GLOBAL
             PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT isMultiConfig)
    if(NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE "" CACHE STRING "")
        set(CMAKE_BUILD_TYPE Release)
        message(STATUS "CMAKE_BUILD_TYPE is undefined. Will default to ${CMAKE_BUILD_TYPE}")
    endif()
    set(allowedBuildTypes Debug Release RelWithDebInfo MinSizeRel)
    if(NOT CMAKE_BUILD_TYPE IN_LIST allowedBuildTypes)
        message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
    endif()
endif()

#=============================
# Project wide setup
#=============================
project(ASD-LAB
        VERSION 1.0.0
        DESCRIPTION "esercizi ASD 2021/22"
        HOMEPAGE_URL "https://github.com/razvnred/UniUD/tree/master/Cavasin/laboratorio%20algoritmi"
        LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

#=============================
# Top level project only
#=============================
if(NOT isMultiConfig)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
                 STRINGS "${allowedBuildTypes}")
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
                 HELPSTRING "Supported build types are: Debug, Release (default), RelWithDebInfo, and MinSizeRel")
endif()

#=============================
# Targets Configuration
#=============================
if(MSVC)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS _CRT_NONSTDC_NO_WARNINGS)
endif()
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    add_compile_options("$<$<CONFIG:Debug>:-Wall;-Wextra;-pedantic>") # ;-Wconversion
endif()

#=============================
# Targets
#=============================
add_executable(12
               12.c
               "utils/utils.c")
add_executable(13
               13.c
               trees.h
               trees.c
               "utils/utils.c")
add_executable(14
               14.c
               trees.h
               trees.c
               "utils/utils.c")
add_executable(15
               15.c
               "utils/utils.c")

#=============================
# Subdirectories
#=============================
add_subdirectory(progetto)
