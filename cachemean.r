# The first function, makeVector creates a special "vector", which is really a list containing a function to:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    
    # First Function "set()"
    # "<<-" is the same as "<-" except it 
    # looks a level up to the parent environment, 
    # in this case makeVector(). Since "m" is 
    # defined within makeVector() NOT in set()
    # so it must look outside of itself.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Second Function "get()"
    get <- function() {
        x
    }
    
    # Third Function "setmean()"
    # This is what helped me out the most
    # I prefer explicit brackets even if not required
    setmean <- function(mean) {
        m <<- mean
    }
    
    # Fourth Function 
    getmean <- function() {
        m
    }
    
    # Return
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# The following function calculates the mean of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of 
# the data and sets the value of the mean in the cache via the setmean function.

cacheMean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


v <- 1:9
a <- makeVector(v)
print(cacheMean(a))
