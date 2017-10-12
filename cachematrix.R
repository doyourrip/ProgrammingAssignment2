## Set a Matrix Object where the inverse can be cached

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  }


## Function creates the inverse using the special matrix from the first makeCashMatrix function

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)){
  
  message("getting cached data")
  return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

