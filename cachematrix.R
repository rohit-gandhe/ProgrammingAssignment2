## The functions here create a matrix and cache its inverse

#Test function to validate that identity matrix can be inversed
testInverse <- function() {
        mm <- NULL
        mm <- matrix(c(1,5,5,1), nrow=2, ncol=2, byrow=TRUE)
        cat("Original matrix:\n")
        print(mm)
        imm <- cacheSolve(makeCacheMatrix(mm))
        cat("inverse:\n")
        print(imm)
        cat("inverse of inverse:\n")
        ioimm <- cacheSolve(makeCacheMatrix(imm))
        print(ioimm)
}

## this function creates a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {

        #make sure invrted matrix is null
        im <- NULL
        
        #set the matrix in the environment ouside the funtion
        set <- function(y){
                x <<- y #assigning y to x
                im <<- NULL #since the matrix has changed, set the inverted matrix to null again
        }
        
        get <- function() x
        
        setInvMatrix <- function(invMat) im <<- invMat
        getInvMatrix <- function() im
        
        #return list of functions
        list(set = set, get=get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInvMatrix()
        
        if(!is.null(im)){
                message("getting cached matrix")
                return(im)
        }
        
        data <- x$get()    # inverse of the matrix has not been calculated so get it
        im <- solve(data)  # invert the matrix
        x$setInvMatrix(im) # cache the inverted matrix
}
