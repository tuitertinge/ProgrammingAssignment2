## Caching the inverse of a matrix

# Inspired by https://class.coursera.org/rprog-013/human_grading/view/courses/973494/assessments/3/submissions

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
+#1.  set the value of the matrix
  +#2.  get the value of the matrix
  +#3.  set the value of the inverse
  +#4.  get the value of the inverse
  
  makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, thencacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("Getting cached data --> Inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
