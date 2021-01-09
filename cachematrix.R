#Your assignment is to write a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
##Instructions for creating a simple matrix and testing the lexical scoping
##https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

makeCacheMatrix <- function(x = matrix()) {
  #sets J to Null
  j <- NULL
  
  set <- function(y) {
    #Set x to a global variable with the values of Y
    x <<- y
    #sets the global variable J to Null usig the <<-
    j <<- NULL
  }
    #assigns the function to get
    get <- function() x
    #assigns the function to setinverse
    setinverse <- function(inverse) j <<- inverse
    #assigns the function to getinverse
    getinverse <- function() j
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    a<-list(set = set, 
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
    print(a)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from cache/memory.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  #This code checks to see if an inverse matrix is already exists in memory and returns the inverse matrix 
  #that is stored in memory
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  #getting the data from the originally assigned matix
  data <- x$get()
  #loops through the matrix to calculate the inverse for each value
  j <- solve(data, ...)
  #add a row of the inverse matrix to x$setinverse
  x$setinverse(j)
  #returns the full inverse matrix
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
    
    