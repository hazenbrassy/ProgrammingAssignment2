## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(solve) { 
      x <<- solve
      m <<- NULL      
    }
    get <- function() {x} ##original value of matrix    
    setMatrix <- function(m) {m <<- solve(m)}
    getMatrix <- function() {m} 
    
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## Returns a matrix that is the inverse 
cacheSolve <- function(x,...) {
  m <- x$getMatrix()
  
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m) 
  }
  
  data <- x$get()
  m <- solve(data)
  x$setMatrix(m)
  m
}


