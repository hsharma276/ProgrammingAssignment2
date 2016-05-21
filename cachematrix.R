## Put comments here that give an overall description of what your
## functions do

## This function will be used to store/cache the inverse value of input matrix

makeCacheMatrix <- function(inputMatrix = matrix()) {

	## initialize variable inverse Matrix
	inverseMatrix <- NULL
        set <- function(y) {
                inputMatrix <<- y
                inverseMatrix <<- NULL
        }
        get <- function() inputMatrix
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function will return the inverse of the input matrix. If that is already 
## calculated earlier will return that value, else will calculate and return.

cacheSolve <- function(inputMatrix, ...) {

        ## check if inverse for this matrix is already available
        inverseMatrix <- inputMatrix$getInverse()
        if (!is.null(inverseMatrix )) {
                ## if yes return the inverse
                message("Matrix is inversed already")
                return(inverseMatrix)
        }
        ## Else create the inverse and return
        matrix<- inputMatrix$get()
        inverseMatrix<- solve(matrix, ...)
        inputMatrix$setInverse(inverseMatrix)
        inverseMatrix
}
