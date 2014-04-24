#The two functions work to cache the inverse of matrix so that if it is needed 
#later on it can be called without having to be recalculated.  In the event 
#it hasn't been cached, it is then evaluated and returned.

#makeCacheMatrix creates the four objects used: set, get, setInverse, getInverse
makeCacheMatrix<-function(x=matrix()) {
  m <- NULL                              #initializes m as NULL, this is later used in cache solve to determine if inverse exists
  set <- function(y) {                   
    x <<- y                              #sets the value
    m <<- NULL                           #sets m to null which will nullify setInverse
  }
  get <- function() x                    #returns the value input when the function was called or the value set
  setInverse <- function(inv) m <<- inv  #sets a value as the matrix inverse
  getInverse <- function() m             #gets the matrix inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve returns the inverse matrix of 'x'
cacheSolve<-function(x, ...){ 
  m <- x$getInverse()                    #looks for the x vector's cache         
  if(!is.null(m)) {                      #checks if there is a cache
    message("getting cached data") 
    return(m)                            #if there is, it returns the cache
  }
  data <- x$get()                        #if there's no cache
  m <- ginv(data, ...)                   #computer inverse matrix.  ginv finds the inverse of a matrix
  x$setInverse(m)                        #saves the result back to x's cache
  m                                      #returns the result
}
