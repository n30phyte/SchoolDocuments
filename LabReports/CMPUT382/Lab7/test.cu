__global__ void total(const float *input, float *output, unsigned int len) {
	__shared__ float shared_data[BLOCK_SIZE];

	unsigned int tx = threadIdx.x;
	unsigned int i = blockIdx.x * (blockDim.x * 2) + tx;

	shared_data[tx] = (i < len) ? input[i] : 0.0f;

	if (i + blockDim.x < len) {
		shared_data[tx] += input[i + blockDim.x];
	}
	__syncthreads();

	for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1)
	{
		if (tx < s)
		{
			shared_data[tx] += shared_data[tx + s];
		}
		__syncthreads();
	}

	if (tx == 0) {
		output[blockIdx.x] = shared_data[tx];
	}
}
