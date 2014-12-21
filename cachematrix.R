



## function makeCacheMatrix enscapulates the input matrix, and its inverse.
## It returns list of a) getter and setter functions associated with the matrix 
## b) getter and setter functions associated with the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    return (x)
  } 
  setinverse <- function(inverse) {
    inv <<- inverse
   
  } 
  getinverse <- function() {
    return (inv)
  } 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## functions cacheSolve accepts as input the list constructed by makeCacheMatrix 
## and returns the inverse of orignal matrix by retrieving the cached value if 
## available, computing if not. In the case inverse it computed, it is cached for
## quicker retrieval in subsequent invocations 

cacheSolve <- function(x, ...) {
      
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return (inverse)
}
