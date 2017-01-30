## cacheMatrix computes the inverse of a matrix but first checks if the solution is the cache.
## If it is, it just returns the cache. If it is not, it does the computation,
## saves it in cache and returns the inverse.

## x is a matrix. 
## makeCacheMatrix takes a matrix, x, and creates a list with the following options: 
## 1) sets the value of the matrix
## 2) gets the value of the marix 
## 3) sets the value of the inverse
## 4) gets the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}



## cacheSolve calls makeCacheMatrix to calculate the value of the inverse if 
## it is not in memory yet. if it is, it return the cached inverse.

cacheSolve <- function(x, ...) {
       i <- x$getInverse()
       if (!is.null(i)){
         message("getting cached inverse matrix")
         return(i)
       }
       data <- x$get()
       i <- solve(data,...)
       x$setInverse(i)
       message("no data cached, running inverse and storing.")
       i
}


