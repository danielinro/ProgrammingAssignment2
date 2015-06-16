## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
  ## Will store the cached inverse matrix
  invmat <- NULL
  ## Set the values of the matrix
  set <- function(y)
  {
    x <<- y
    invmat <<- NULL
  }
  ## Get the values of the matrix
  get <- function() x
  ## Set the values of the inverse matrix
  setinv <- function(inverse) invmat <<- inverse
  ## Get the values of the inverse matrix
  getinv <- function() invmat
  ## Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...)
{
  ## Get the inverse matrix of x matrix
  invmat <- x$getinv()
  # If inverse is cached
  if (!is.null(invmat))
  {
    ## Show where do we get data
    message("Matrix got from cache")
    ## Return cached matrix
    return(invmat)
  }
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  invmat <- solve(data, ...)
  # Cache the inverse
  x$setinv(invmat)
  ## Show where do we get data
  message("Matrix calculated")
  # Return it
  invmat
}
