## Overall, this function find the inverse of an invertible matrix. If already calculated, the inverse matrix is retrieved from cache.
## If not, it is calculated for the first time which might take time and then is chached.
## 

## Here, we create a constructor function that creates a list of objective functions to be called out in the next function.
## The inverse of the matrix is also cached here.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y = matrix()) {
    inverse <<- NULL
    x <<- y
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This fuction recieves a matrix as an argument and compare it with the previous matrix (if any) saved in an environment (by makeCacheMatrix function).
## If the matrix has already processed, it returns the cached result and, if not, this function processes the matrix for the first time.

cacheSolve <- function(env, data = matrix(), ...) {
  DataInCache <- env$get()
  inverse <- env$getinverse()
  if (!is.null(inverse)
      & identical(data, DataInCache)) {
    message("getting cache data")
    return(inverse)
  }
  else {
    message("This data set is being procsessed for the first time.
            This process might take time.")
    inverse <- solve(data, ...)
    env$set(data)
    env$setinverse(inverse)
    return(inverse)
  }
}
