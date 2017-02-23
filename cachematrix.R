
## The makeCacheMatrix() function returns an R object of type
## makeCacheMatrix that stores a matrix and its inverse.
## The cacheSolve() takes as an argument a makeCacheMatrix object
## and retrieves its inverse from the cache if it exists.
## If if does not exist it creates it and and calls setsolve to store
## the inverse in the makeCacheMatrix object

## Create a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) im <<- solve
      getsolve <- function() im
      list(set = set,
            get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## Return the inverse if it is cached. If it is not cached then create it
## and set it in the makeCacheMatrix object and return it.

cacheSolve <- function(x, ...) {
        im <- x$getsolve()
        if(!is.null(im)) {
            message("getting cached matrix")
            return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setsolve(im)
        im
}
