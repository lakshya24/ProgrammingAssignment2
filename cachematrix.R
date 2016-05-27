####Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
####than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
####Your assignment is to write a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set the matrix function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the matrix function
  get <- function() x 
  #set inverse of matrix function
  setInv <- function(inverse) inv <<- inverse
  #get inverse of matrix function
  getInv <- function() inv
  #create list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  #check if teh inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if not,get the original matrix
  matrixDat <- x$get()
  #calculate inverse
  inv <- solve(matrixDat, ...)
  #set inverse in teh cache
  x$setInv(inv)
  inv
}