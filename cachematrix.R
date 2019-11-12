## Combination of makeCacheMatrix() and cacheSolve can compute the inversion of a matrix
## and store it in the cache; so when the inversion of a matrix is needed, it can be 
## retrieved directly from the cache if has been computed before

## The model function specify the variable matrix and the process of set matrix value, get
## matrix value, set the inverted matrix value, get the inverted matrix value

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y){
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## The user-interactive function that take in the return value of makeCacheMatrix() to return
## inverted matrix either from cache if has been computed before, or from computing it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if (!is.null(inv_x)){
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
