## This function return a list of 4 untility functions to cache a matrix
## The matrix to be cached can be any matrix that is precomputed

## This function has a list of 4 utility function
## get -- retrieve original input matrix
## set -- overwrite with a new input matrix
## getMatrix -- retrieve the computed matrix
## setMatrix -- set computed Matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){
                mat <- NULL
                x <<- y
        }
        get <- function() x
        getMatrix <- function() mat
        setMatrix <- function(input_mat) mat <<- input_mat
        list(set = set, get = get, getMatrix = getMatrix, setMatrix = setMatrix)
}


## This function computed the matrix inverse using the function solve()
## And store the computed matrix in cache. A subsequent retrieval of inverted matrix
## is returned from cache

cacheSolve <- function(x, ...) {
        inverted <- x$getMatrix()
        if(!is.null(inverted)){
                message("getting cached data")
                return (inverted)
        }
        data <- x$get()
        inverted <- solve(data)
        x$setMatrix(inverted)
        inverted
}
