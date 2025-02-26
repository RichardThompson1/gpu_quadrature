#!/bin/bash
SECONDS=0

#THIS BATCH IS TIME VARYING
# 2**12 X 2**12 X 2**8 = 2**32 additions
./build/gpu_quadrature -x 4096 -y 4096 -t 256
./build/gpu_quadrature -x 4096 -y 4096 -t 256 -n 3 -m 3
./build/gpu_quadrature -x 4096 -y 4096 -t 256 -n 7 -m 7
./build/gpu_quadrature -x 4096 -y 4096 -t 256 -n 17 -m 17

# 2**12 X 2**12 X 2**14 = 2**38 additions
./build/gpu_quadrature -x 4096 -y 4096 -t 16384
./build/gpu_quadrature -x 4096 -y 4096 -t 16384 -n 3 -m 3
./build/gpu_quadrature -x 4096 -y 4096 -t 16384 -n 7 -m 7
./build/gpu_quadrature -x 4096 -y 4096 -t 16384 -n 17 -m 17

# 2**12 X 2**12 X 2**20 = 2**44 additions
./build/gpu_quadrature -x 4096 -y 4096 -t 1048576
./build/gpu_quadrature -x 4096 -y 4096 -t 1048576 -n 3 -m 3
./build/gpu_quadrature -x 4096 -y 4096 -t 1048576 -n 7 -m 7
./build/gpu_quadrature -x 4096 -y 4096 -t 1048576 -n 17 -m 17

#THIS BATCH IS DENSITY VARYING
# 2**11 X 2**11 X 2**10 = 2**32 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 1024
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 17 -m 17

# 2**14 X 2**14 X 2**10 = 2**38 additions
./build/gpu_quadrature -x 16384 -y 16384 -t 1024
./build/gpu_quadrature -x 16384 -y 16384 -t 1024 -n 3 -m 3
./build/gpu_quadrature -x 16384 -y 16384 -t 1024 -n 7 -m 7
./build/gpu_quadrature -x 16384 -y 16384 -t 1024 -n 17 -m 17

# 2**17 X 2**17 X 2**10 = 2**42 additions
./build/gpu_quadrature -x 131072 -y 131072 -t 1024
./build/gpu_quadrature -x 131072 -y 131072 -t 1024 -n 3 -m 3
./build/gpu_quadrature -x 131072 -y 131072 -t 1024 -n 7 -m 7
./build/gpu_quadrature -x 131072 -y 131072 -t 1024 -n 17 -m 17

echo "This batch of runs took $SECONDS seconds to run."


# Script run with 2**32 2**32 2**38 2**38 2**42 2**42 = 2**45.088 additions took  seconds