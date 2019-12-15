## R programming - WEEK 3 - Programming assignment


# 1 - Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y){
    x <<- y
    c <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) c <<- inverse
  getInverse <- function() c 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# 2 - Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  c <- x$getInverse()
  if(!is.null(c)){
    message("getting cached data")
    return(c)
  }
  matrix <- x$get()
  c <- solve(matrix,...)
  x$setInverse(c)
  c
}