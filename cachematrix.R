## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {

## set the value of the matrix
  
  inv <- NULL
  
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  
## get the value of the matrix
  get <- function() x
  
## set the value of the Inverse
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  
## get the value of the Inverse
  
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
#Attempt to get the cached Inverse Matrix from makeCacheMatrix object
  
  inv <- x$getInverse()
  
## If the Inverse Matrix already previously cached by the setInverseMatrix() then get the 
##  the Inverse MAtrix from the cache

  if(!is.null(inv)){
  
  message("getting cached data")
  return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

