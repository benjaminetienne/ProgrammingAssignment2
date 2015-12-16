##makeCacheMatrix calculates the inverse of a matrix, stores the result in the cache so it can be accessed faster.
##cacheSolve returns the inverse of a matrix : if the inverse has already been calculated and stored in the cache, we get the desired value using 'setinverse'
##Otherwise, we calculate the inverse, store the result in the cache, and return the inverse.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#This function calculates and stores the inverse of a matrix in the cache
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
  
        #//Sets the special matrix 
        set <- function(A){
                x <<- A
                inverse <<- NULL
        }
  
        #//Gets the special matrix
        get <- function() x
        
        #//Sets the matrix's inverse
        setinverse <- function(solve_m) inverse <<- solve_m
        
        #//Gets the matrix's inverse
        getinverse <- function() inverse
        
        #----List of all functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#This function looks for the inverse of a matrix in the cache
#and returns it if it exists, otherwise calculates it and stores it 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cacheSolve <- function(x, ...) {
        
        #//We get the inverse of M in the cache
        inverse <- x$getinverse()
        
        #//If this inverse is not NULL, we return it
        if(!is.null(inverse)){
                message('Inverse is in cache, here it comes.')
                return(inverse)
        }
        
        #//If the inverse has not been calculated yet, we calculate it and save it in the cache using "setinverse()"
        mat <- x$get()
        inverse <- solve(mat)
        x$setinverse(inverse)
        inverse
}
