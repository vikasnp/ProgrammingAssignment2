###	Caching the Inverse of a Matrix

##	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



## Here a Matrix object capable of caching is created.

makeCacheMatrix <- function(x = matrix()) 
{
    
	inverse <- NULL
                                                            
# set is used to edit the matrix and also it clears the cache
    set <- function(y) 
	{
        x <<- y
        inverse <<- NULL
    }

# get is used to return the raw matrix
    get <- function() 
	{
        x
    }

# setInverse sets the inverse variable
    setInverse <- function(i) 
	{
        inverse <<- i
    }

# getInverse gets the cached inverse
    getInverse <- function() 
	{
        inverse
    }

# return the special matrix
    list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.      
##If the inverse is already available in cache it is simply returned.
 
cacheSolve <- function(x, ...) 
{
    # get the cached inverse
    inverse <- x$getInverse()
     

	 # if inverse of a matrix is already available it is returned from cache.
    if(!is.null(inverse)) 
	{
        message("Inverse available in cache")
        return(inverse)
    }

    # Calculate inverse and then store it in cache
    matr <- x$get()
    inverse <- solve(matr, ...)
    x$setInverse(inverse)

    return(inverse)
}

# SAMPLE OUTPUT
# > matr$set(matrix(5:8, 2, 2))
# > cacheSolve(matr)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(matr)
# Inverse available in cache
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > matr <- makeCacheMatrix(matrix(3:6, 2, 2))
# > cacheSolve(matr)
# [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
# > cacheSolve(matr)
# Inverse available in cache
# [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
# > matr$set(matrix(5:8, 2, 2))
# > cacheSolve(matr)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(matr)
# Inverse available in cache
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > 
