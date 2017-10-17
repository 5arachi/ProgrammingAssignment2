#This function returns a list of function from the input matrix 
#which can be used for setting and getting the original matrix, 
#and also for setting and gettting the inverse of the matrix. 
makeCacheMatrix <- function( x = matrix()){
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<-i
        getInverse <- function() inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function calculates the inverse of a special matrix 
#that is an output of makeCachematrix and returns the inverse of the original matrix,
#the inverse of the matrix is only calculated if it was not calcualted before. 
cacheSolve <- function(x, ...){
        inverse_x <- x$getInverse()
        if (!is.null(inverse_x)){
                message("getting cached inverse of x")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data)
        x$setInverse(inverse_x)
        inverse_x
}