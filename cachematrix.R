##
##  This is a pair of functions that cache the inverse of a matrix.
##
##  makeCacheMatrix: This function creates a special "matrix" object
##                   that can cache the original matrix and its inverse.
##
##  cacheSolve:      This function computes the inverse of the special 
##                   "matrix" returned by makeCacheMatrix. 
##                   If the inverse has already been calculated (and 
##                   the matrix has not changed), then cacheSolve
##                   retrieves the inverse from the cache.
##
##  makeCacheMatrix creates a list containing functions to:
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of the inverse matrix (i.e.'solves' the matrix)
##    - get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
	  # s is the 'inverse' matrix and it is reset to NULL every 
	  # time makeCacheMatrix is called
      s <- NULL 				
			
	  # The 'set' function saves the input matrix in cache,
	  # and resets the inverse matrix to NULL.
      set <- function(y) {
            x <<- y 
            s <<- NULL
      }
	  # The 'get' function returns the value of the original matrix
      get <- function() x  
	  
	  # The 'setsolved' function calculates the inverse of the input  
	  # matrix and stores the value in cache using superassignment.
	  # It is called by cacheSolve() during the first access.
      setsolved <- function(solve) s <<- solve   
	  
	  # The 'getsolved' function will return the cached value to 
	  # cacheSolve() on subsequent accesses.
      getsolved <- function() s 
      
	  # Finally, create a list of the internal functions ('methods') 
	  # so that cacheSolve() knows how to access them.
      list(set = set, get = get,
            setsolved = setsolved,
            getsolved = getsolved)
}

##  cacheSolve calculates the inverse of the matrix created with
##  makeCacheMatrix. However, it first checks to see if the inverse has
##  already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it calculates the inverse of the matrix and sets the
##  value of the inverse in the cache via the setsolved function.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      s <- x$getsolved()
      
	  # check to see if the inverse has already been calculated
      if(!is.null(s)) {
            # if yes, get the inverse from the cache and skip the computation
            message("getting cached matrix")
            return(s)
      }
	  # otherwise get the original matrix,
      data <- x$get()
	  
	  # calculate its inverse matrix,
      s <- solve(data, ...)
	  
	  # and store it in cache using the setsolved method of the CacheMatrix
      x$setsolved(s)
      s
}