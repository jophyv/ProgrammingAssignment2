## Functions to calculate inverse of a matrix and re-use the results stored in cache for re-runs
#  Description of individual functions below;

## Function 1 of 2 - Function for generating special matrix object.

makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(y)
	{
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
#-------------------------------------------------------------------------------------------

##Function 2 of 2 -Compute the inverse of the special "matrix" returned by makeCacheMatrix above
#This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 

{

	inv<-x$getmatrix()
  	if(!is.null(inv))
		{
    			message("getting cached data")
    			return(inv)
  		}
  	matrix<-x$get()
  	inv<-solve(matrix, ...)
  	x$setmatrix(inv)
  	inv
}