## This R file contains a pair of functions that cache the inverse of a matrix.
## The first function, makeCacheMatrix, will return a "special matrix object" that allows you to cache its inverse
##The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix. However, if the inverse has been saved to the cache, then the cached inverse will be returned

## The makeCacheMatrix is a function that takes in a matrix as its only paramter and returns a special object. This object is essentially a list which contains the 4 methods needed to either cache the value of an inverse matrix, or retrieve the cached value of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## instantiating inv to null
  inv <- NULL
  ## defining the function for storing the matrix data & inverse in the cache
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  ## function that returns the cached matrix data
  get <- function() x
  ## function that stores the inverse matrix in the cache
  setInverse <- function(inverse) inv <<- inverse
  ## function that returns the inverse matrix from the cache
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes in a special 'matrix' object and then extracts all of the pieces of information necessary to compute the inverse of the matrix
## If the matrix has already had its inverse calculated, and cached, then the cached value will be retrieved from memory
## If the matrix does not have an inverse cached, then it will extract the matrix data from this special object, calculate the inverse, and then store the value in the cache
## A matrix will be returned by this function

cacheSolve <- function(x, ...) {
  ## instantiating the inverse variable to whatever is stored in the cache for object 'x'
  inv <- x$getInverse()
  ## check if the inverse was in the cache by comparing to null
  if(!isNull(x)){
    message("gettingf cached data")
    return(inv)
  }
  
  ## Will not reach the code below if the above conditional is true
  
  ## funtion to retrieve cached matrix data
  matrix <- x$get()
  ## calculating the inverse
  inv <- solve(matrix)
  ## storing calculation in the cache
  x$setInverse(inv)
  ## return the inverse matrix
  inv
}
