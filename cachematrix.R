## Constructing a helper function to hold 4 special matrix functions in a list.
##  1. Function to set matrix
##  2. Function to get matrix
##  3. Function to set inverse matrix
##  4. Function to get inverse matrix
## Recieves a matrix as input parameter and returns a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    # Value for inverse matrix
    inv <- NULL
    
    # 1st function sets the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # 2nd function returns the matrix
    get <- function() {
        x
    }
    
    # 3rd function assigns the inversed matrix to inv variable
    setInverse <- function(inversedMatrix) {
        inv <<- inversedMatrix
    }
    
    # 4th function returns inversed matrix
    getInverse <- function() {
        inv
    }
    
    # Return the list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function checks if inversed matrix is already calculated and returns
## its value. If there is no value for inversed matrix it performs the calculation,
## assigns the result to inversedMatrix variable and returns it. This function uses 
## the makeCacheMatrix function for getting and setting original and inverse matrices.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversedMatrix <- x$getInverse()
    ## check if the inverse matrix is already calculated and return it
    if(!is.null(inversedMatrix)) {
        message("getting cached data")
        return(inversedMatrix)
    }
    ## get original matrix
    data <- x$get() 
    ## calculate inverse matrix
    inversedMatrix <- solve(data)
    ## store inverse matrix to variable
    x$setInverse(inversedMatrix)
    #3 return inversed matrix
    inversedMatrix
}

# uncomment to test the code!!
# print("Test 1:")
# print("Creating 2x2 matrix for inversion...")
# m <- rbind(c(1, -1/4), c(-1/4, 1))  
# print(m)
# print("Creating a 'special' matrix function... ")
# a <- makeCacheMatrix(m)
# print(names(a))
# print("Inverting a matrix for 1st time...")
# res <- cacheSolve(a)
# print(res)
# print("Inverting a matrix for 2nd time/getting cached value...")
# res <- cacheSolve(a)
# print(res)
# print("Checking solution by multiplying original and inverse matrix...")
# print(round(solve(m)%*%m), 4)
# print("--------------------")
# print("Test 2:")
# print("Creating 3x3 matrix m for inversion...")
# m <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0 ), 3, 3)
# print(m)
# print("Creating a 'special' matrix function... ")
# a <- makeCacheMatrix(m)
# print(names(a))
# print("Inverting a matrix for 1st time...")
# res <- cacheSolve(a)
# print(res)
# print("Inverting a matrix for 2nd time/getting cached value...")
# res <- cacheSolve(a)
# print(res)
# print("Checking solution by multiplying original and inverse matrix...")
# print(round(solve(m)%*%m), 4)