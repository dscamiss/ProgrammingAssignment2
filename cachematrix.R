# this source file implements:
# - a "matrix"-type object with matrix inverse caching
# - an interface function to extract cached matrix inverses (if they exist)

# makeCacheMatrix():
# - represents a "matrix"-type object with matrix inverse caching
# - object is represented as a function; returns list of four "member functions"
# - X argument can be updated/accessed with set()/get() functions
# - X^{-1} can be updated/accessed with set_inv()/get_inv() functions

makeCacheMatrix <- function(X = matrix()) {
  # cached copy of X^{-1}
  Xinv <- NULL
  
  # update X argument, reset X^{-1}
  set <- function(Y) {
    X <<- Y
    Xinv <<- NULL
  }
  
  # return X argument
  get <- function() X
  
  # update X^{-1} 
  set_inv <- function(Y) Xinv <<- Y
  
  # return Xinv
  get_inv <- function() Xinv
  
  # return list of "member functions"
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

# cacheSolve():
# - computes and returns the inverse X^{-1} of the invertible "matrix" X
# - X must be an instance of the makeCacheMatrix() function
# - if X^{-1} has been cached, then the cached version is returned 
# - if X^{-1} has not been computed, then it is computed and returned
# - optional arguments to inversion routine solve() can be passed as varargs

cacheSolve <- function(X, ...) {
  # get Xinv 
  Xinv <- X$get_inv()
  
  # check for existence of "cached" Xinv; if it exists, then return it
  if (!is.null(Xinv)) {
    message("getting cached data...")
    return(Xinv)
  }
  
  # if cached Xinv does not exist, then compute it, cache it, and return it
  Y <- X$get()
  Xinv <- solve(Y, ...)
  X$set_inv(Xinv)
  Xinv
}
