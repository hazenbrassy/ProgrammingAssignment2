##makeCacheMatrix returns an object that has functions that will 
##cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(solve) { 
      x <<- solve
      m <<- NULL      
    }
    get <- function() {x} ##original (passed in) value of matrix    
    setMatrix <- function(m) {m <<- solve(m)} ## caches inverse of matrix
    getMatrix <- function() {m} ## returns cached version of matrix
    
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

##1)  need to pass it the object that makeCacheMatrix creates
##2) checks cache to see if it exists and returns cached object if true
##3) otherwise gets the inverse and caches it via the setMatrix method of the object
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


