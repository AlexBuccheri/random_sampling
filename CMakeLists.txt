cmake_minimum_required(VERSION 3.20)
project(PRNGExploration VERSION 1.0.0 LANGUAGES Fortran CXX)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# Fortran shared library
add_library(fortran_rand)

add_subdirectory(src/fortran)

set_target_properties(fortran_rand
        PROPERTIES
        VERSION "${PROJECT_VERSION}"
        SOVERSION "${PROJECT_VERSION_MAJOR}"
        LIBRARY_OUTPUT_NAME fortran_rand
        LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}"
        )

# CPP test
add_executable(lemire_mapping src/cpp/lemire_mapping.cpp)
