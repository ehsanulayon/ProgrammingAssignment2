## Put comments here that give an overall description of what your
## functions do
## Week 3 assignment
## The main objective of this function is to shorten the time needed for calculating matrix inverse by caching the matrix

## Write a short comment describing this function
## makeCacheMatrix is a function that takes a matrix as input and gets the inverse matrix. Also it can cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL                                    #initialize invm as null
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x                                   #get the matrix value
  setinverse <- function(inverse) invm <<- inverse      #set the invertible matrix value
  getinverse <- function() invm                         #get the invertible matrix value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve function at first checks whether the matrix inverse has already been calculated or not. If calculated, then it returns the cached value. Otherwise, it calculates the inverse and returns the value.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          invm <- x$getinverse()
  if(!is.null(invm)) {                                 
    message("getting cached data")                      #if invm is null then type message
    return(invm)                                        #return the value of invm
  }
  value <- x$get()
  invm <- solve(value, ...)                             #solve funtion to calculate inverse
  x$setinverse(invm)
  invm                                                  #return the value of invm
}
