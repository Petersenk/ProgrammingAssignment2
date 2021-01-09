#Your assignment is to write a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
##Instructions for creating a simple matrix and testing the lexical scoping
##https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
##Testing the Functions
#create the matrix
myMatrix <- matrix(c(1,2,3,4),2,2)

myMatrix
#cache the matrix
cacheMatrix <- makeCacheMatrix(myMatrix)

#retrieve the inverse cache matrix
cacheSolve(cacheMatrix)


#Alan's simple matrices example
    m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow=2, ncol=2)
    m1

#cache the matrix
    cacheMatrix <- makeCacheMatrix(m1)

#retrieve the inverse cache matrix
    cacheSolve(cacheMatrix)

#cache the matrix
    n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)


#cache the matrix
    cacheMatrix <- makeCacheMatrix(n1)

#retrieve the inverse cache matrix
    cacheSolve(cacheMatrix)

#checks
    n1 %*% m1
    m1 %*% n1
    
    