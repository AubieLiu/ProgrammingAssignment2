## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creats a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <-function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## , then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


## Function testing

my_matrix <- makeCacheMatrix(matrix(5:8, 2, 2)) 

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

my_matrix$getInverse()

my_matrix$set(matrix(c(5, 8, 10, 15), 2, 2))

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)

my_matrix$getInverse()
