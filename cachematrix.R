## These functions cache the inverse of a matrix so the inverse does not have to be computed over and over again.
## Instead it reads from cache.
## makeCacheMatrix takes a matrix as input and creates a list with four elements, which are functions. And it stores 
## the original matrix and wat will be the inverse matrix, initially set to NULL
## two of the four functions create these values(matrixes) and the other two read them.
##
## Testing:
## inputmatrix <- matrix(c(0,1,1,0),2,2)
## my_matrix<-makeCacheMatrix(inputmatrix)
## cachesolve(my_matrix)
##     [,1] [,2]
## [1,]    0    1
## [2,]    1    0
## cachesolve(my_matrix)
## getting cached data
##     [,1] [,2]
## [1,]    0    1
## [2,]    1    0
##
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## cachesolve gets the value of reverse matrix either by computing or reading from cache
## 
cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

