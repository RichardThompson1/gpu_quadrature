
cmake_minimum_required(VERSION 3.15)

project(GpuQuadrature LANGUAGES Fortran)

set(CMAKE_Fortran_COMPILER "/opt/nvidia/hpc_sdk/Linux_x86_64/24.5/compilers/bin/nvfortran")

set(CUDA_LIB_DIR "/opt/nvidia/hpc_sdk/Linux_x86_64/24.5/cuda/12.4/targets/x86_64-linux/lib")

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cuda -gpu=cuda12.4 -Minfo=accel")

include_directories("/opt/nvidia/hpc_sdk/Linux_x86_64/24.5/cuda/12.4/targets/x86_64-linux/include")

#tells cmake to treat .cuf as fortran files
set_source_files_properties(parallel_addition.cuf PROPERTIES LANGUAGE Fortran)

#add_executable(get_gpu_info get_gpu_info.f90)
add_executable(gpu_quadrature main.f90 quadrature.cuf)

# Attempt to find CUDA runtime library
find_library(CUDART_LIBRARY NAMES cudart HINTS ${CUDA_LIB_DIR})
message(STATUS "CUDART_LIBRARY found at ${CUDART_LIBRARY}")

# If the CUDA library is found, link it; otherwise print an error message
if(NOT CUDART_LIBRARY)
  message(FATAL_ERROR "CUDA libraries not found")
else()
  target_link_libraries(gpu_quadrature ${CUDART_LIBRARY})
endif()