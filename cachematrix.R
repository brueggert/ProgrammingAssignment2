## makeCacheMatrix has the following functions, each returning a list
# 1. Set the value of the matrix (set)
# 2. Get the value of the matrix (get)
# 3. Set the value of the inverse of the matrix (setinverse)
# 4. Get the value of the inverse of the matrix (getinverse)
makeCacheMatrix <- function(x = matrix()) {
  # inv stores the cached inverse of the matrix
  inv <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse of the matrix
  getinverse <- function() inv
  
  # return matrix as list through the above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve returns the inverse of the matrix by: 
## 1. checking to see if it has already been cached,
##     and if so skip computation and use cached values
## 2. if it has not been cached, then the inverse of the 
##    matrix is then computed and returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # if the inverse has already been cached, use cached values
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if the inverse is not cached, compute it
  data <- x$get()
  inv <- solve(data)
  
  # cache the inverse of the matrix
  x$setinverse(inv)
  
  # return cached value of the inverse
  inv
  
}
