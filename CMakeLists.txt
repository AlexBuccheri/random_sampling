cmake_minimum_required(VERSION 3.20)
project(PRNGExploration VERSION 1.0.0 LANGUAGES Fortran CXX)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(test_xor
        src/fortran/test_xor.f90
        src/fortran/xorshifts.f90)

add_executable(hidden_shuffle src/fortran/hidden_shuffle.f90)

add_executable(lemire_mapping src/cpp/lemire_mapping.cpp)
