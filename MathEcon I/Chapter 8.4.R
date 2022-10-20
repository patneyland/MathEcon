# MATRIX
A<-matrix(c(1,2,3,
            2,-1,1,
            3,0,-1),
         nrow=3,ncol=3,byrow=TRUE)
print(A)

B<-matrix(c(1,5,10,
            1,5,10,
            1,5,10),
          nrow=3,ncol=3,byrow=TRUE)
print(B)

C<-matrix(c(1,0,0,
            0,1,0,
            0,0,1),
          nrow=3,ncol=3,byrow=TRUE)
print(C)

# SCALAR
r<-10

# SCALAR ADDITION/SUBSTRACTION
print(A+r)

# SCALAR MULTIPLICATION
print(A*r)

# MATRIX ADDITION/SUBSTRACTION
print(A+B)

# MATRIX MULTIPLICATION
print(A%*%B)
print(A%*%C)

# TRANSPOSE
print(t(A))
print(t(B))

# INVERSE OF A
A.inv<-solve(A)
print(A.inv)

# PROPERTY OF INVERSES
print(round(A%*%A.inv))
print(round(A.inv%*%A))

# RANK OF A
qr(A)$rank

# DETERMINANT OF A MATRIX (Chapter 9)
det(A)
det(B)

# You can check all the other matrix rules and properties
# using the code provided above.


