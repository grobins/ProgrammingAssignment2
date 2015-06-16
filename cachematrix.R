## Below are two functions.  The first will create a cached version of a matrix
## in the parent environment, such that the matrix can be referred to by other 
## functions.  The second will retrieve the inverted form of this stored 
## matrix, or if NULL will instead set it.

## makeCacheMatrix creates a list of four functions.  
## set() will set the initial state of variables x and m as the passed in 
##   matrix and NULL respectively. 
## get() will retrieve the value of x in the local environment
## setmatrix() will set the value of x in the cache so that it can be passed
##   to other functions.
## getmatrix() will retrieve this cached value of x.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve)  m<<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve returns in the inverted form of the matrix stored in the cache by
## the function makeCacheMatrix.  First it will check if the matrix stored by
## makeCacheMatrix is NULL.  If it is not NULL, cacheSolve will return the inverted
## cached matrix.  If it is NULL, cacheMatrix will take the local stored matrix
## and save it using the makeCacheMatrix function.  When run a second time, it will
## then retrieve this stored matrix.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }  
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}


#Test Results: 
# m <- makeCacheMatrix()
# m$set(matrix(c(1:4),2,2))
# m$get()
# cacheSolve(m)
# cacheSolve(m)
 
