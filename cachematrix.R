## There are two function in this code. makeCacheMatrix and cacheSolve
##  The first function, makeCacheMatrix creates a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

## Example to run the function:
#> testmatrix<-matrix(rnorm(9),nrow=3,ncol=3)  # for a given square invertable matrix
#> test<-makeCacheMatrix(atestmatrix)
#> cacheSolve(test) #to get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function creates the inverse of the matrix (from the above function) after first checking
##if the inverse has been previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
