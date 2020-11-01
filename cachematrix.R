## Put comments here that give an overall description of what your
## functions 

## The makeCacheMatrix function takes in a matrix (x) and creates a "matrix" object with:
##  - the matrix data of x
##  - funciton set() that takes in matrix y and sets the matrix in the object to y
##  - function get() that returns the matrix x of the object
##  - function setinverse() that takes in a matrix and caches it as the inverse matrix of x
##  - function getinverse() that returns the cached value of inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv_m <<- inv
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes in a "matrix" object created by the makeCacheMatrix function and does the following:
##  - checks if the inverse matrix of the object's matrix is cached
##  - if yes, returns the inverse matrix
##  - if no, compute the inverse matrix using solve(), caches  the inverse matrix and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)              
  x$setinverse(inv_m)
  inv_m        
}
