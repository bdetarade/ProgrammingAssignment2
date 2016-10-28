## This code provide an OBJECT 'makeCacheMatrix' and a function 'cacheSolve'
## which aim to optimize the matrix inversion process

## An object with a matrix as parameter for the constructor
## This object provide four methodes
## - set
## - get
## - setSolve (set the inversion)
## - getSolve (get the inversion)


makeCacheMatrix <- function(x = matrix()) {
  ## the inverse of a matrix x is computed by the solve() function
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {
    x
  }
  setSolve <- function(solve) {
    s <<- solve
  }
  getSolve <- function() {
    s
  }
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## A singleton which get the inversion of a matrix if its already done
## If not it compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}

