
# R program to find inverse of a Matrix

# Create 3 different vectors
# using combine method.
a1 <- c(3, 3, 3, 1)
a2 <- c(2, 4, 5, 2)
a3 <- c(3, 4, 5, 1)
a4 <- c(2, 2, 3, 4)

# bind the three vectors into a matrix 
# using rbind() which is basically
# row-wise binding.
A <- rbind(a1, a2, a3, a4)

# print the original matrix
print(A)

# Use the solve() function 
# to calculate the inverse.
T1 <- solve(A)

# print the inverse of the matrix.
print(T1)

