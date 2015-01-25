##  Functions to cache the inverse of a matrix and retrieves from cache for repeated execution.
#   Description of individual functions below;

## function to generate special matrix object
}

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) inv<<- solve
  getmatrix<-function() inv
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## function to compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}