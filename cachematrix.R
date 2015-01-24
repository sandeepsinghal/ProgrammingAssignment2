# This function exposes functions to return the original matrix 
# It contains a variable called xInv that will act 
makeCacheMatrix <- function(x = matrix()) {
    
    # Inverse of X
    xInv <- NULL
    
    # Setter on the matrix, reset the inverse too
    set <- function (y){
        x <<- y
        xInv <<- NULL
    }
    
    # Return the matrix 
    get <- function(){
        x
    }
    
    setInverse <-  function (inv) {
        xInv <<- inv
    }
    
    # Return the cached inverse
    getInverse <- function (){
        xInv
    }
    
    list (set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Return a matrix that is the inverse of 'x'
## returns the inverse of a matrix if it is in the cache
## If it is not present in the cache, then the inverse is computed , put in the cache and then returned
cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()
    
    # If the cache is null, then compute the matrix and set the cache for retrieval next time
    if (is.null(i)){
        print("Cache Empty, solving the matrix : ")
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
    }
    
    i
}
