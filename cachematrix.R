### Nested functions (effectively, classes) used to calculate and store the 
### cache of a matrix so that subsequent calls need only access the stored 
### version rather than recalculating.

## Create "class" to store matrix information
makeCacheMatrix <- function(x = matrix()) {
  # Start with an empty declaration for the inverted matrix
  inv <- NULL
  
  # Set x as the called matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # Show x (original) matrix when called
  get <- function() x
  
  # Set solved matrix from cacheSolve into place holder inv
  setinv <- function(invMat) inv <<- invMat
  
  # Show inv
  getinv <- function() inv
  
  # Set list of functions into makeCacheMatrix function/"class"
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Run necessary calculations on makeCacheMatrix data
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # If inverse already stored, get it.
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  # Otherwise, calculate and store inverse
  inv <- solve(x$get())
  x$setinv(inv)
  
  message("Inverse matrix cached...")
  inv
}

##### Use
# m$set(matrix(rnorm(100000000),nrow=1000,ncol=1000))  # set matrix
# a <- cacheSolve(m)  # Solve it
# a <- cacheSolve(m)  # Recall it
#####

##### Breaking down example code
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
#
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
#
##### Use:
# v <- makeVector()  # build "class" object
# v$set(rnorm(100))  # set vector with v$set(x)
# v$get()  # View vector stored in object
# cachemean(v)  # Commit mean to cache
# cachemean(v)  # Again to view cache
#####



