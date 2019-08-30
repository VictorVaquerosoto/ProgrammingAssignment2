## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix:
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix:
  get <- function() x
  #set the value of the inv:
  setsolve <- function(solve) m <<- solve
  #get the value of the inv:
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) { #If the cache is not empty, we return the inverse.
    message("getting cached data")
    return(m)
  }
  #If the cache is empty,we calculate the inverse, cache it and return it.
  data <- x$get()
  #The only difference (apart from the name of the functions) 
  #with respect to the example is here:
  m <- solve(data, ...) #Calculate the inverse with the function solve()
  x$setsolve(m)   
  m #return the inverse of the matrix
}
