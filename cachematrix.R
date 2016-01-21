## This R file contains two functions that are meant to be used in order
## to cache the value of the inverse of a matrix since the operation of
## inverting a matrix is an expensive one. Therefore having fast access 
## to a cached value can speed up the process of obtaining the result.


## makeCacheMatrix takes a matrix as input and it produces a list of
## functions as output. This list of functions  allow to:
## i) set(y): the value of the matrix passed to makeCache in to a variable x of parent environment 
## of the set(y) function. This parent environment is the environment of makeCacheMatrix()
## ii) get(): gets the matrix passed to makeCache. In this function x is a free variable,
## therefore R searches in the parent environment that is the environment of makeCache
## iii) setInverse(inverse) sets the inverse matrix passed to the function, to a variable (inv) of the
## parent environment. Since the inv variable is present in the makeCache environment, it is 
## set there.
## iiii) getInverse(): gets the inverse matrix. In this function inv is a free variable,
## therefore R searches in the parent environment that is the environment of makeCache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve requires as input an object (list of functions) produced by makeCacheMatrix(x).
## The cached inverse matrix value is retrieved calling getInverse() (from the list of function passed in input to makeMatrix), 
## if succeed, the cached value is returned; else the original matrix is retrieved via the get() 
## method (from the list of function passed to makeMatrix).
## The inverse of the original matrix is computed with the R solve(data) function.
## The computed inverse is then set in the environment of makeCacheMatrix via the (special function from the list of function passed in input to makeMatrix) setInverse(inv).
## Finally the inverse matrix is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
