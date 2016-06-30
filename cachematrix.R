## These two functions create a special matrix
##and caches its inverse.

## This function creates an empty matrix if not passed to it
##and returns a list of 4 functions which: 1)set and cache
##the matrix; 2) return the matrix; 3) set the inverse of
##the matrix; 4) return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  I <- NULL
  
  set <- function(y) {
    
    x <<- y
    I <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get, 
       setinverse=setinverse, getinverse = getinverse)
  
}

## This function accepts the list returned by the above function
##and extracts the necessary function to calculate the inverse
##of the matrix contained in the list's environment. If the
#matrix is singular or is not a square matrix then a message will
##be displayed and the inverse is not computed

cacheSolve <- function(x, ...) {
   
  I <- x$getinverse()
  
  if(!is.null(I)) {
    
    message("getting cached data")
    return(I)
    
  }
  
  data <- x$get()
  
  if(nrow(data) == ncol(data) && det(data) != 0){
    
    I <- solve(data, ...)
    x$setinverse(I)
    return(I)
    
  }else message("The matrix is either singular or is not a square matrix")
  
}
