## These functions work together to create a special matrix object that stores
## a matrix and solves and caches its inverse

## This function creates a list that contains functions to set the value of a 
## matrix, get its value, set the value of the inverse of a matrix and get
## the value of the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) 
    
    {
    
    # Initialize the value of the inverse of the matrix to null
    I  <-  NULL
    
    # This is the function to set the value of the matrix
    
    set <- function(y)
    {
        I <<- NULL # set the matrix inverse to NULL since it has not yet 
        # been calculated
        x <<- y #sets the value of the matrix
        
    }
    
    # This is the function that gets the value of the matrix
    
    get  <- function() 
    {
        x #returns the value of the matrix 
    }
    
    # This is the function to set the inverse of the matrix to the 
    # argument given in this function
    setInverse <- function(inverse)
    {
        I <<- inverse
        
    }
    
    # This is the function that returns the value of the inverse of the matrix
    
    getInverse <- function()
    {
        # If the inverse has been calculated and stored, it will return thr value
        # If the inverse has not been calculated and/or stored, it will return NULL
        I 
    }
    
    # Returns a list of the functions defined above 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## The function below returns the inverse of the matrix created with the function
## above. If the inverse has not been calculated already, it computes it and 
## stores it in the special matrix object


cacheSolve <- function(x, ...) 
    
    {
    # Get the value of the inverse to check if it exists 
    I <- x$getInverse()
    
    # If the inverse has already been calculated, skip the calculation and return
    # the value of the inverse
    
    if(!is.null(I))
    {
        message("getting cached data")
        return(I)
    }
    
    # The following occurs if the value of I is NULL i.e. no inverse has been 
    # stored for this matrix
    
    # Retrieve the matrix itself
    mat  <-  x$get()
    
    # calculate the inverse of the matrix
    I  <- solve(mat, ...)
    
    # Set the inverse of the matrix and store it in the special cache matrix 
    # created
    
    x$setInverse(I)
    
    # Return the inverse of the matrix
    
    I
}
