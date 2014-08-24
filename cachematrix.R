## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 


## The below function
## 1) set the values of the matrix
## 2) get the values of the matrix
## 3) set the values of the inverse
## 4) get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y 
		i<<- NULL
		}
	get <- function () x
	setinverse <- function(inverse) 
	i <<- solve
	getinverse <- function() i
 	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()	
	if(!is.null(i)) {
		message("getting cached data.")
		return(i)
		}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i  
	}
