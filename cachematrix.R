## Put comments here that give an overall description of what your
## functions do

## This function is making cache for matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y)
	{
		x <<- y #setting nev matrix to x
		m <<- NULL #clear m (inverse of matrix x)
	}
	get <- function() x #when running whatever with get() return x (matrix)
	setinverse <- function(inverse) m <<- inverse #setting inverse matrix to m
	getinverse <- function() m #when running whaterever with getinverse() return m (inverse of matrix)
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is checking if in cache exists inverse of our matrix; if it exists then
## will return inverse of matrix from cache;
## if not, then will calculate and return new inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #get inverse of matrix
        if(!is.null(m)) #checking if inverse of this matrix exists in cache
        {
        	message("getting cache data")
        	return(m)
        }
        data <- x$get() #set to data our matrix
        m <- solve(data, ...) #set to m calculated inverse of matrix
        x$setinverse(m) #set inverse of matrix to x
        m #return inverse of matrix
}
