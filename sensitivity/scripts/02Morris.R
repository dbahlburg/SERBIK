k <- 10
p <- 4
delta <- p/(2*(p-1))

#1. Create matrix B which decides which input variable will be varied
B <- matrix(nrow = k + 1,
            ncol = k)
B[lower.tri(B)] <- 1
B[!lower.tri(B)] <- 0

#2. Define a vector containing possible values for the input variables
setValues <- c(0,1/3,2/3,1)

#3. Define starting point/intial input variable constellation
initialX <- sample(setValues, k, replace = T)
#initialX <- matrix(sample(setValues, noVariables, replace = T),ncol = 1)

#4. Create D which contains randomly picked -1/1 on its diagonal, otherwise 0
D <- diag(sample(c(-1,1),k, replace = T), nrow = k, ncol = k)

#5. Create matrix J ((k+1)xk matrix of 1s)
J <- matrix(nrow = k + 1, ncol = k, data = 1)

#6. Create permutation matrix P (kxk) which contains one randomly placed 1 in each column but no two columns have 1 in the same position
P <- matrix(nrow = k, ncol = k, data = 0)
P[sample(1:k, k) + c(0, 1:(k - 1) * k)] <- 1

#5. Calculate Sampling matrix Bstar
Bstar <- (matrix(rep(initialX, times = k + 1), ncol = k, byrow = T) + 
            0.5 * delta * (2 * B - J) %*% D + J) %*% P




          