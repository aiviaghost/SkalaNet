#define min(a, b) (a < b ? a : b)

#define T 10

void mult(int n, int m, int p, const float A[], const float B[], float res[]) {
    for (int I = 0; I < n; I += T) {
        for (int J = 0; J < p; J += T) {
            for (int K = 0; K < m; K += T) {
                for (int i = I; i < min(I + T, n); i++) {
                    for (int j = J; j < min(J + T, p); j++) {
                        for (int k = K; k < min(K + T, m); k++) {
                            res[i * p + j] += A[i * m + k] * B[k * p + j];
                        }
                    }
                }
            }
        }
    }
}
