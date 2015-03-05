## These two functions work together to implement a caching
## strategy for matrix inversion: one object wraps the original
## matrix type inside an enriched objects (i.e. it adds a
## 'property' which is used to store the inverse of itself).
## The other function is used to access the inverse of the
## matrix and it has to be used instead of solve() function
## to invert the given matrix

## This function wraps a matrix object by storing the matrix
## together with its inverse. The inverse of the matrix is
## set from the outside, allowing a lazy computation.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
    message("setting inverse to nil")
  }
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}


## This function returns the inverse of the given matrix (created
## with makeCacheMatrix()). The inverse is computed only the first
## time, as it is stored inside the given matrix for subsequent calls

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if( !is.null(i) ){
    message("returning cached inverse")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
