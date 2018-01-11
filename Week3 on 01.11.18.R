



##     The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##     
##     First assume that the inputs will be a square matrix function called "x."
##     Let "s" be the solve variable, representing the inverse of the matrix.
##     The makeCacheMatrix function  sets "s" to NULL and defines the function that will 
##     be used for the calculation in the cacheSolve function below.

makeCacheMatrix <- function(x = numeric()) {
  # Set "s" to NULL.  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
    initally.
  }
  get <- function() x
  setsolv <- function(solve) s <<- solve
  ##   Above you are creating the equation for inverting the matrix.
  getsolv <- function() s
  ##   Now you have created the function to solve for "s" with "s" as NULL per line 7.
  ##   And the elements of the  list are saved in cache.
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
}

##    The cacheSolve function computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
#     has not changed), then the cachesolve should retrieve the inverse from the cache.
##    
##    The cacheSolve function checks to see if there is a value "s" for the inverse. 
##    If there is, then it recalls it from the cache, and if not then it calculates it.

cacheSolve <- function(x, ...) {
  ##  go into cache and check if "s" is not NULL,
  ##  if it is, then print a comment and the value, if not then go to the next step.
  s <- x$getsolv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ##  Since "s" was NULL in the cashe from the previous makeCacheMatrix function on line 7, 
  ##  you then solve for "s" and print the result. One can check this statement 
  ##  by putting in a number on line 7 instead of NULL.
  data <- x$get()
  s <- solve(data)
  x$setsolv(s) 
  s 
}


## This can be used to test the work above

B = matrix(c(2, 4, 3, 1, 5, 7,4,8,10),nrow=3,ncol=3)
x <- makeCacheMatrix(B)

cacheSolve(x)

## Validate results

solve(B)



