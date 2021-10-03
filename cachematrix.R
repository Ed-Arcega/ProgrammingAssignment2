## The following pair of functions lets users create a matrix and easily
## perform Matrix Inversion onto it. The inverted matrix is then "cached" 
## within the environment of the first function so it can be quickly recalled
## without computing it repeatedly.

## The "makeCacheMatrix" function lets one create a special "matrix" object
## where its inverse can be stored.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The "cacheSolve" function either computes the inverse of the special matrix
## above to be stored or retrieves an already cached inverse matrix if the matrix
## has not been changed.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached inversed matrix")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
