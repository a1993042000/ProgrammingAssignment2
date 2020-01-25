## Put comments here that give an overall description of what your functions do


## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the matrix
#get the matrix
#set the inverse matrix
#get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

#The cacheSolve calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse matrix has already been calculated. 
#If so, it gets the inverse matrix from the cache and skips the computation. 
#Otherwise, it calculates the inverse matrix and sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if (!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}


## Testing
m1 <- matrix(c(2,2,-3,4),2,2)
m1c <- makeCacheMatrix(m1)
cacheSolve(m1c)
cacheSolve(m1c)