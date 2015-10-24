## Put comments here that give an overall description of what your
## functions do

## This function operates like an advanced memory cell for stored Inverse value

makeCacheMatrix <- function(x = matrix()) {
  #print(x)
  m <- NULL
  get <- function() x  
  setsolve <- function(solve_) m <<- solve_
  getsolve <- function(){ print(m) m }
  list(get = get, setsolve = setsolve,
       getsolve = getsolve)  
}

## Main function responsible for getting job (do Inverse versus matrix provided) done. 
## Checks if Inverse value is already calculated and resore it. Otherwise perform calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$getsolve()
  print(result)
  
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }  
  
  #If Mean was not stored
  data <- x$get()
  result <- solve(data, ...)
  x$setsolve(result)
  result <- x$getsolve()
  result
}

B = matrix(c(2, 4, 3, 1, 5, 7, 8, 4), nrow=3, ncol=3) 
cacheSolve(makeCacheMatrix(B))

