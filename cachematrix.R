## Create getters & setters for the matrix & the inverse
## Cache the inverse if not already cached otherwise return from cache

## Make cache matrix

makeCacheMatrix <- function(x = matrix()) {

  ## initialize inverse
  inverse <- NULL

  ## set the matrix
  set <- function(y) {
    x <<- y
    ## nullify inverse since the matrix changed
    inverse <<- NULL
  }

  ## return the matrix
  get <- function() {
    x
  }

  ## set inverseMatrix
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }

  ## return the inverseMatrix
  getInverse <- function() {
    inverse
  }

  ## create a 'list' of functions
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)

}


## Calculate & return inverse of the matrix
## if already calculated return from cache else calculate & return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get the inverseMatrix
  i <- x$getInverse()

  ## inverseMatrix  exists; return from cache
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }

  ## no inverseMatrix in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
