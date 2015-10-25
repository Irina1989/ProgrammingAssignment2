## the two functions together check if the result of a computation 
## has already been done and is stored in an object to be retrieved, 
## otherwise it will do that computation

## the makeCacheMatrix function creates an object that can store 
## the inverse of the matrix, from this object we can retrieve the inverse
## later so we don't have to compute it a second time

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x <<- y
			m <<- NULL
}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## the cacheSolve function checks if the inverse has already been 
## computed and if not, solves for the inverse and passes it on to the 
## storing object created by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)){
				message("getting cached data")
				return(m)
}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
