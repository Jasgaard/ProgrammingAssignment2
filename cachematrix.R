## Programming Assignment 2 (peer assesment): Lexical Scoping
## The Programming Assignment is devided in two parts:
## 1) Cache a Matrix that can be used to repeatably solve the inverse of a Matrix
## 2) Calculate the inverse of a Matrix by using the solve function 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cashedInverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    cashedInverseMatrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) cashedInverseMatrix <<- solve
  getmatrix <- function() cashedInverseMatrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getmatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setmatrix(inverseMatrix)
  inverseMatrix  
}