## This function is used to create 4 functions to manipulate a matrix
## The first function is used to assign a values to a matrix to be manipulated
## The second allows you to pull the existing matrix data
## The third allows you to calculate the inverse of a matrix and cache it
## The fourth allows you to pull the existing cached inverse if there is one
## To use, define a symetric matrix
## ex: matrix(1:9, 3, 3) defines a 3x3 with 1 thru 9
## assign a variable equal to the function; you may use your matrix as an argument or enter the data later
## ex: a <- makecachematrix()
## to assign a new matrix use a$set(matrix(1:9, 3, 3)) or some other data
## this is all that's needed 
## proceed to next function below

makeCacheMatrix <- function(x = matrix()) {
     
     m <- NULL                                    ## initializes local variable
     set <- function(y) {                         ## first function in a list of 4 functions
          x <<- y                                 ## allows you to enter new matrix data
          m <<- NULL                              ## clears any residual m variable in cache
     }
     get <- function() x                          ## second function in list
                                                  ## pulls the matrix entered as an argument
     
     setmatrix <- function(solve) m <<- solve     ## third function in list
                                                  ## sets (not calculates) an inverse value and caches it
     
     getmatrix <- function() m                    ## pulls the value of m if it exists in cache
     
     list(set = set, get = get, 
          setmatrix = setmatrix,
          getmatrix = getmatrix)                  ## creates the final list of the above functions
                                                  ## I don't understand why we need this

}


## This funchtion returns the inverse of a matrix entered into the makecachematrix function or pulls the existing inverse from cache

cacheSolve <- function(x, ...) {
    
     m <- x$getmatrix()                           ## checks if there's an inverse value in cache
     if(!is.null(m)) {                            ## if there is a value, execute this function
          message("getting cached data")          ## print a message
          return(m)                               ## print the value in cache
     }
     data <- x$get()                              ## if there isn't an inverse value in cache, get the matrix data
     m <- solve(data, ...)                        ## calculate the inverse
     x$setmatrix(m)                               ## cache the inverse value
     m                                            ## return the inverse value
     
}
