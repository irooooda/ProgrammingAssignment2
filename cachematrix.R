## Creates a special object to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # stores the inverse of the matrix

# set() assigns a new matrix to x and resets inv to NULL(matrix is changed)
  set <- function(y) {
    x <<- y # store the matrix
    inv <<- NULL # reset the inverse when matrix changed
  }
# get() stores the matrix x
  get <- function() x 
# setInverse() stores the inverse of the matrix in inv
  setInverse <- function(inverse) inv <<- inverse
# getInverse() retrieves the cached inverse from inv
  getInverse <- function() inv
    
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computes the inverse if not cached
## or retrieves the cached inverse if already computed

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # retrieve the cached inverse (if it exists)
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # else calculate the inverse
  mat <- x$get()         # Get the matrix
  inv <- solve(mat, ...) # Calculate the inverse using solve()
  x$setInverse(inv)      # Cache the inverse
  inv       
}
