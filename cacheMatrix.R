#Takes matrix and caches the inverse
makeCachematrix <- function(m = matrix()) { 
    #Set the value of the matrix
    inv <- NULL
    set <- function(y) {
            m <<- y
            inv <<- NULL
    }
    #Get the value of the matrix
    get <- function() m
    #Set the value of the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    #Get the value of the inverse of the matrix
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}

#Calculate/Retrieve inverse of matrix
cacheSolve <- function(m, ...){
    #Get the value of the inverse of the matrix
    inv <- m$getinverse()
    if(!is.null(inv)){
        message("Getting cached data!")
        return(inv)
    }
    #Calculate inverse of the matrix if NULL
    data <- m$get()
    inv <- solve(data, ...)
    m$setinverse(inv)
    return(inv)
}