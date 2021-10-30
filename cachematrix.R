## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" 
##object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ##set value of matrix
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      ##  <<-
      ##It's the 'superassignment' operator.  It does the assignment in the
      ##enclosing environment. That is, starting with the enclosing frame, it
      ##works its way up towards the global environment until it finds a
      ##variable called , and then assigns to it.   If it never finds
      ##an existing one it creates one in the global environment.
      
      ##get value of matrix
      get <- function() {x}
      ##set & get value of inverse
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set,get = get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## we inverse a matrix and store the results if same matrix is again
## given as input already calculated inverse matrix is returned from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
  }
