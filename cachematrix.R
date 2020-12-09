## The following functions are designed to allow the caching of matrix inversion

##makeCacheMatrix function will first create a special matrix object that can
##cache the inverse
##When this function finishes executing, it returns a list containing the 
##the 4 functions that will be accessed through the $ operator in cacheSolve function

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ##Initialized
  set <- function(y){
    x <<- y ##used to assign a value in an environment that is different from the current environment
    inv <<-NULL 
  }
  get <- function() x
  setinverse <- function(inv_value)inv <<- inv_value
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve function utilizes the arguments created from the makeCacheMatrix
##function. It checks if there is a inverse value that is already cached in the 
##above function. If there is, and the matrix has not been altered, cacheSolve
##is able to retrieve the cached result.
##If there is no cached value, it will calculate the inverse matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
