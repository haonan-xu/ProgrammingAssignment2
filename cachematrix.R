## Programming Assignment 2: Lexical Scoping
## Last Edited: 03 Aug 2020

## The following script creates two functions that 
## utilize the lexical scoping of R to store and retrieve
## the inverse of a square matrix from cache

## "makeCacheMatrix" creates special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is a square matrix
  
  ## Returns a list of four functions (sepcial "matrix") to
  ##    1. set the value of the matrix
  ##    2. get the value of the matrix
  ##    3. set the value of the inverse
  ##    4. get the value of the inverse
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" function computes inverse of the special
## "matrix" returned by "makeCacheMatix". If the inverse has
## been calculated, it simply retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  ## 'x' is the output of "makeCacheMatrix" function
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
