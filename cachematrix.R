## These two functions create the inverse of a matrix

##References: https://rpubs.com/BIN_FANG/112261

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                  #sets m (the mean value) to null
  set <- function(y) {                       #starts the set function   
    
    x <<- y                                  #assigns x the matrix that is in Y   
    m <<- NULL                               #sets the m function to NULL   
  }
  get <- function() x                        #start the get function
  setinverse <- function(solve) m <<- solve  #the setinverse function is created to set the inverse of the mean
  getinverse <- function() m                 #  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)      
  x$setinverse(m)            #sets the inverse of the matrix
  m                         #passes the inverse of the matrix back
}


a <- diag(5,3)  #creates a matrix that can be passed to the makeCacheMatrix function
a #prints the matrix
cachedMatrix <- makeCacheMatrix(a)
cacheSolve(cachedMatrix)