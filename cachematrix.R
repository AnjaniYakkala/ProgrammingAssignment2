## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : The function creates a special "matrix" object 
## that can cahce its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  
  m<- NULL
  set<- function(y) 
  {
    x<<- y
    m<<- NULL
    
  }
  get <- function() x
  setSolveInverse <- function(SolveInverse) m<<- SolveInverse
  getSolveInverse<- function() m
  list(set=set, get=get, setSolveInverse=setSolveInverse, getSolveInverse=getSolveInverse)
  
}


## CacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. if the inverse
##has already been calculated and the matrix has not chnaged, 
## then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m<- x$getSolveInverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data,...)
  x$setSolveInverse(m)
  m
}
