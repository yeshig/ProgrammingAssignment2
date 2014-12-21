## 
## makeCacheMatric outputs a list of functions that act on the matrix
## mInverse is the variable for matrix inverse that is cached
## x is the matrix. The user will need to pass a square matrix y
## when using the set function to create the matrix data x.
## As example, use the following commands to set and get the matrix x:
## x.mat$set(matrix(c(2, 4, 3, 1, 5, 7, 9, 7, 2), nrow=3, ncol=3) )
## x.mat$get()
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(matrixInverse) mInverse <<- matrixInverse
  getMatrixInverse <- function() mInverse
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}


## cacheSolve calculates the inverse of the matrix. It reads the 
## cached mInverse. If mInverse is null, the function calculates 
## the inverse using solve.
## As an example, call the function by passing the above x.mat as follows
## matrixInverse <- cacheSolve(x.mat)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getMatrixInverse()
  if(!is.null(mInverse)) {
    message("getting cached matrix inverse data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setMatrixInverse(mInverse)
  mInverse
}
