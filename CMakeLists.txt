cmake_minimum_required(VERSION 3.20)
project(PRNGExploration VERSION 1.0.0 LANGUAGES Fortran CXX)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

add_library(fortran_rand)

set_target_properties(fortran_rand
        PROPERTIES
        VERSION "${PROJECT_VERSION}"
        SOVERSION "${PROJECT_VERSION_MAJOR}"
        LIBRARY_OUTPUT_NAME isdff90
        LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}"
        )

target_sources(fortran_rand PRIVATE
       src/fortran/reservoir_sampling.f90
       src/fortran/integer_mapping.f90
       src/fortran/xorshifts.f90
       )

#add_executable(hidden_shuffle src/fortran/hidden_shuffle.f90)

#add_executable(fisher_yates
#        src/fortran/fisher_yates.f90
#        src/fortran/integer_mapping.f90
#        src/fortran/xorshifts.f90)

# CPP
add_executable(lemire_mapping src/cpp/lemire_mapping.cpp)
