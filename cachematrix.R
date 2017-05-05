## Function makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.
## Function cacheSolve computess the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
# has not changed), then the function  retrieves the inverse from the cache.

## Function makeCacheMatrix creates a special "matrix", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(myInv) inv <<- myInv
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function cacheSolve calculates the inverse of the given (special) matrix.
## If the inverse already exists in cache it will return tha cached value. 
## Otherwise it will calculate it and store the outcome via the setInv function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
        }
        data <- x$get()
	inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

