setwd("~/Desktop/Coursera")
## Put comments here that give an overall description of what your
## functions do


## Creating a function which makes a matrix object "x" whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL ## No inversion of x
  set <- function(y)  {
    x <<- y   ## superassignment for parent environment
    inv <<- NULL ## No inversion
  }
  get <- function() x
  setinverse <- function(inversematrix)  inv <<- inversematrix
  getinverse <- function(inversematrix)  inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  }


## Creating a function which evaluates and takes the inverse of the above matrix obect "x"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv))   {  ##if there are no reasons why "x" cannot be inverted, then proceed
    message("getting cached data") 
    return(inv) 
  }
  data <- x$get()  
  inv <- solve(inv) ##set inv to the inverse of matrix object "x"
  x$setinverse(inv)
  inv
}
