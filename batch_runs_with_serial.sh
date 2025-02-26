#!/bin/bash
SECONDS=0

#THIS GROUP IS TIME VARYING
# 2**11 X 2**11 X 2**5 = 2**27 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 32
./build/gpu_quadrature -x 2048 -y 2048 -t 32 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 32 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 32 -n 17 -m 17

# 2**11 X 2**11 X 2**9 = 2**31 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 512
./build/gpu_quadrature -x 2048 -y 2048 -t 512 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 512 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 512 -n 17 -m 17

# 2**11 X 2**11 X 2**10 = 2**32 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 1024
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 1024 -n 17 -m 17

#THIS GROUP IS DENSITY VARYING
# 2**11 X 2**11 X 2**7 = 2**29 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 128
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 17 -m 17

# 2**12 X 2**12 X 2**7 = 2**31 additions
./build/gpu_quadrature -x 4096 -y 4096 -t 128
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 17 -m 17

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 8192 -y 8192 -t 128
./build/gpu_quadrature -x 8192 -y 8192 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 8192 -y 8192 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 8192 -y 8192 -t 128 -n 17 -m 17

echo "\n\nThis batch of runs took $SECONDS seconds to run."


# Script run with 2**27 2**27 2**31 2**31 2**34 2**34 = 2** additions took 1536 seconds