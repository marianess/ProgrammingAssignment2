## makeCacheMatrix() creates a matrix and stores its inverse
## This function creates an enviroment that include any
## object defined in makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            # assign the input argument to x object
            # in the parent environment
            x <<- y
            m <<- NULL
      }
      get <- function() x
      # calculate an inverse
      setinv <- function(mat) m <<- solve(mat)
      getinv <- function() m
      # assign each of the function
      # as an element within a list
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}

### cacheSolve() requires an argument returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      # test whether object value is NULL
      # if TRUE return the object
      if(!is.null(m)){
            message('getting cached inversse')
            return(m)
      }
      # if FALSE inverse of a matrix
      # is calculated from the input pbject
      # and returned to the parent environment
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}


# 
# 
# makeVector <- function(x = numeric()) {
#       m <- NULL
#       set <- function(y) {
#             x <<- y
#             m <<- NULL
#       }
#       get <- function() x
#       setmean <- function(mean) m <<- mean
#       getmean <- function() m
#       list(set = set, get = get,
#            setmean = setmean,
#            getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#       m <- x$getmean()
#       if(!is.null(m)) {
#             message("getting cached data")
#             return(m)
#       }
#       data <- x$get()
#       m <- mean(data, ...)
#       x$setmean(m)
#       m
# }
# 
# 












