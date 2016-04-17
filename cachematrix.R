##makeCacheMatrix function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  #m is the matrix
  m <- NULL
  #set establishes the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get gets the value of the matrix
  get <- function() x
  #set sets the value of the inverse of the matrix m
  setinverse <- function(inverse) m <<- inverse
  #get gets the value of the inverse of the matrix m
  getinverse <- function() m
  # puts all four functions into a list that will be included in the object that makeCacheMatrix will be assigned to 
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve function computes the invese of the special "matrix" returned by the makeCachematrix
##if the inverse has been already calculated, the cached value for inverse will be used

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # if statement checks to see if an inverse for the matrix has already been calculated
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if no cached inverse exists, a new inverse will be calculated
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
