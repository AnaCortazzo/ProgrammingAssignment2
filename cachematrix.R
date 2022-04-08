# MakeCacheMatrix creates an R object (a list) containing 4 functions: 
    # setMatrix assign new value to x in parent environment and reset 
        # the value of inver in case it has any stored value.
    # getMatrix takes the value of x from the parent environment
    # setInverse changes the value of the inverse in parent environment
    # getInverse assigns the value of the inverse set by setInverse

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL 
    setMatrix <- function(y){
        x <<- y 
        inver <<- NULL 
        }
    getMatrix <- function() x 
    setInverse <- function(inv) inver <<- inv
    getInverse <- function() inver
    
    list(set = setMatrix, get = getMatrix, setInverse = setInverse, 
         getInverse = getInverse) #create a named list
}


# cacheSolve takes the object created by MakeCacheMatrix. 
# First it checks if an inverse (cache) value already exists. 
# If TRUE, it returns that value. If FALSE it calculates the inverse 
# and stores it in the object created by MakeCacheMatrix.

cacheSolve <- function(x, ...) {
    inver <- x$getInverse()
    
    if (!is.null(inver)) {
        message("obtaining cache inverse")
        return(inver)
    } else {
        matriz <- x$get()
        inver <- solve(matriz) #solve inverse for matrix
        x$setInverse(inver) #store the result
        inver
    }
}
