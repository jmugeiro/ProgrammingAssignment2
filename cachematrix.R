##When running time consuming computations, it's good to cache the results so That You can look up later Them instead of computing Them again. 
##The Following functions can compute and cache the inverse of a matrix.
 

## makeCacheMatrix (): creates a special "matrix" que object can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  
  
  inv <- NULL
  
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){x
  }
  
  setinv <- function(i){
    inv <<- i
  }
  
  getinv <- function(){inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
  
  
}


## cacheSolve (): computes the inverse of the "matrix" . 
##If the inverse has already Been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
 
  
  
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
  
}



jcm <- matrix(c(1,4,2,6,7,89,90,7,6),
              ncol=3, nrow=3,
              dimnames=list(NULL, c("dom", "sab", "fer")))


a<-makeCacheMatrix()

a$set (jcm)
a$get()
## Return a matrix that is the inverse of 'x'
cacheSolve(a)
