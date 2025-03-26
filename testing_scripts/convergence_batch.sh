#!/bin/bash
SECONDS=0


# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 32 -y 32 -t 128
./build/gpu_quadrature -x 32 -y 32 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 32 -y 32 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 32 -y 32 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 32 -y 32 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 32 -y 32 -t 128 -n 2047 -m 2047

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 64 -y 64 -t 128
./build/gpu_quadrature -x 64 -y 64 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 64 -y 64 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 64 -y 64 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 64 -y 64 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 64 -y 64 -t 128 -n 2047 -m 2047

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 128 -y 128 -t 128
./build/gpu_quadrature -x 128 -y 128 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 128 -y 128 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 128 -y 128 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 128 -y 128 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 128 -y 128 -t 128 -n 2047 -m 2047

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 256 -y 256 -t 128
./build/gpu_quadrature -x 256 -y 256 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 256 -y 256 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 256 -y 256 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 256 -y 256 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 256 -y 256 -t 128 -n 2047 -m 2047

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 512 -y 512 -t 128
./build/gpu_quadrature -x 512 -y 512 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 512 -y 512 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 512 -y 512 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 512 -y 512 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 512 -y 512 -t 128 -n 2047 -m 2047

# 2**13 X 2**13 X 2**7 = 2**34 additions
./build/gpu_quadrature -x 1024 -y 1024 -t 128
./build/gpu_quadrature -x 1024 -y 1024 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 1024 -y 1024 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 1024 -y 1024 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 1024 -y 1024 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 1024 -y 1024 -t 128 -n 2047 -m 2047

#THIS GROUP IS DENSITY VARYING
# 2**11 X 2**11 X 2**7 = 2**29 additions
./build/gpu_quadrature -x 2048 -y 2048 -t 128
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 2048 -y 2048 -t 128 -n 2047 -m 2047

# 2**12 X 2**12 X 2**7 = 2**31 additions
./build/gpu_quadrature -x 4096 -y 4096 -t 128
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 3 -m 3
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 7 -m 7
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 17 -m 17
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 127 -m 127
./build/gpu_quadrature -x 4096 -y 4096 -t 128 -n 2047 -m 2047



echo "\n\nThis batch of runs took $SECONDS seconds to run."
