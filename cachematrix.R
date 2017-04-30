# makeCacheMatrix() and cacheSolve() work together to compute the inverse of a matrix and cache
# the result so that the inverse need not be computed each time the inverse is required. To
# call the inverse from cache or calculate it for the first time, use cacheSolve(). Before the
# initial use of cacheSolve(), makeCacheMatrix() will need to be called and supplied a matrix
# for an argument.

# makeCacheMatrix takes an argument in the form (c(range_of_numbers)), nrow, ncol)
# or matrix(variable_name) where variable name is a previously initialized matrix constructed
# using that syntax. If nrow!=ncol, your matrix will not be invertible.
# e.g. makeCacheMatrix(matrix(c(1:4),2,2)) or makeCacheMatrix(matrix(runif(4),2,2))

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL # initializes the matrix to be calculated
        set <- function (y) {
                x <<- y # x gets y in the parent environment when set is called
                inverse <<- NULL #clears a cached inverse matrix when set is called 
        }
        get <- function() x #retrieves x from the parent environment
        setinverse <- function(solution) inverse <<-solution #inverse gets solutions in the parent environment
        getinverse <- function() inverse #retrives inverse from the parent environment
        madeMatrix <<- list ( #returns a list of named functions cachesolve() can use after this function completes
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


# cacheSolve() will return a matrix that is the inverse of 'x' when supplied an invertible matrix; if the inverse
# has already been calculated cacheSolve() will return the cached value. If cacheSolve() needs to calculate the
# inverse, it will attempt to filter out input containing NA values and non-invertible matrices before calculating
# the inverse.

cacheSolve <- function(x=madeMatrix, ...) { #point this function to the output of makeCacheMatrix()
        inverse <- x$getinverse() #retrieve the value of inverse
        if(!is.null(inverse)) { #check if inverse is not NULL
                message("getting cached data") #if inverse is not NULL, message that the cache is being retrived
                return(inverse) #return the cached non-NULL value
        } # if inverse is NULL, continue to caclulate the inverse
        invertible <- x$get() #get the invertible matrix to be calculated
        if (anyNA(invertible)) { #check if the matrix contains an NA value
                stop("Bad news! This matrix contains an NA value.") #exit with message
        }
        if (det(invertible)==0) { #quick check if the matrix is invertible (more or less)
                stop("Bad news! This matrix is not invertible.") #exit with message
        }
        inverse <- (solve(invertible, ...)) #solve for the inverse of the invertible matrix
        x$setinverse(inverse) #set the value of inverse to the calculated value
        inverse #return the value of the inverse to the parent environment        
}