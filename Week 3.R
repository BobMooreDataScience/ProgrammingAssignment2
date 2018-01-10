makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()# this takes the vector x and calls it data
  m <- solve(matrix(data,ncol=2,byrow=TRUE)) #this gets the inverse of the data
  x$setmean(m) # this sets the mean "m" to this column
  m # This prints the mean "m"
}


x <- makeCacheMatrix(c(51,43,22,92))



cacheSolve(x)

