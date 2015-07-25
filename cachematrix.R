## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(original_matrix = matrix()) {
   ## Default value for inverse_matrix
   inverse_matrix <- NULL
   
   ## Setter of the original matrix.
   ## When a new original matrix is set, then the previous value of 
   ##    inverse_matrix has to be descarded.
   set <- function(new_original_matrix) {
      original_matrix <<- new_original_matrix
      inverse_matrix <<- NULL
   }
   
   ## Getter of the original matrix.
   get <- function() original_matrix
   
   ## Setter of the inverse matrix.
   setInverse <- function(new_inverse_matrix) inverse_matrix <<- new_inverse_matrix
   
   ## Getter of the inverse matrix.
   getInverse <- function() inverse_matrix
   
   ## Return the defined functions
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {
   ## Get cached value of inverse matrix.
   inverse_matrix <- cacheMatrix$getInverse()
   ## If the cached value is not null, then return it.
   if(!is.null(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
   }
   
   ## If the inverse matrix in not cached yet, then we have to calculate it.
   ## Get the original matrix.
   original_matrix <- cacheMatrix$get()
   ## Calculate the inverse matrix based on the value of the original matrix.
   inverse_matrix <- solve(original_matrix, ...)
   ## Cache the calculated inverse matrix.
   cacheMatrix$setInverse(inverse_matrix)
   ## Return the inverse matrix.
   inverse_matrix
}
