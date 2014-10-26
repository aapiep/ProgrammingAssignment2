## Put comments here that give an overall description of what your
## functions do

# Create a matrix object which can also contain the inverse of the matrix (a computationally expensive operation which it makes sense not to repeat unless needed)
# The inverse, once calculated, will be kept inside the object and only recalculated if the matrix has been changed.
# For simplicity, the matrix is assumed to be invertible. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {	#store a value and reset the cached inverse value
                x <<- y 	
                i <<- NULL
        }
        get <- function() x		# get the stored matrix value
        setinverse <- function(inverse) i <<- inverse	#store the inverse 
														#(note: the calculation is done by the cacheSolve function, so we could really be storing anything we like here)
        getinverse <- function() i						#get the stored inverse
        list(set = set, get = get,						
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
		# for a 'special' matrix object as defined above, 
		# cache and return its inverse, i.e. 
			# if it has not been calculated for this value of the matrix (first time or upon change): calculate it ('solve' function) and store it inside the object
			# otherwise just return the stored value for the inverse
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# Example run:
#
# > A = matrix( c(2, 4, 3, 1, 5, 7, 10, 10, 8), nrow=3, ncol=3, byrow = TRUE)
# > b <- makeCacheMatrix(A)
# > b$get()
     # [,1] [,2] [,3]
# [1,]    2    4    3
# [2,]    1    5    7
# [3,]   10   10    8
# > b$getinverse()
# NULL
# > c<-cacheSolve(b)
# > b$getinverse()
           # [,1]        [,2]        [,3]
# [1,] -0.4411765 -0.02941176  0.19117647
# [2,]  0.9117647 -0.20588235 -0.16176471
# [3,] -0.5882353  0.29411765  0.08823529
# > 
