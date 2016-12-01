## These fxns will allow us to cache an inverted matrix.

## makeCacheMatrix - Takes a matrix x, and stores it it's parent environment. 
## It also returns 2 pairs of getter/setter functions we will use to store/update x and inverted x in the parent env.

makeCacheMatrix <- function(x = matrix()) {     ## At this line, we've initialized the matrix x in our parent env.
        x_inv <- NULL                           ## Initialize the var our cacheSolve fxn will later return, x_inv
        
        set <- function(y) {                    ## Fxn 1: Our setter of the matrix, x. New x means reset x_inv.
                        x <<- y                 ## x, the matrix to be inverted, is set to be y.
                        x_inv <<- NULL          ## x_inv is set to null. The << operator means x & x_inv are in parent env
        } 
        get <- function() x                     ## Our first getter returns x from the parent env (see line 10)
        
        setInv <- function(inv) x_inv <<- inv   ## Setter 2. Pass inv (see cacheSolve) to the parent env's x_inv
        getInv <- function() x_inv              ## Getter 2. Return the x_inv var from our parent env
        
        list( set = set,       get = get,
              setInv = setInv, getInv = getInv) ## Finally, return the fxns made above as the output of makeCacheMatrix.
}


## cacheSolve will invert the matrix stored above and pass it's inverse into our cache.
## It takes x as an arg, where x is the output of makeCacheMatrix (NOT the x matrix passed to the above fxn.)

cacheSolve <- function(x, ...) {
        x_inv <- x$getInv()
        if(!is.null(x_inv)) {                   ## If x_inv is non null, this retrieves it.
                message("Fetching cached inv.")
                return(x_inv)
        }
        
        m_to_invert <- x$get()                  ## Fetch the matrix we want to invert (it was x in the above fxn.)
        x_inv <- solve(m_to_invert, ...)        ## Solve the inverse of our matrix.
        x$setInv(x_inv)                         ## Set our new inverse matrix to the parent env.
        x_inv                                   ## Return the inverted matrix (also cached now) as our fxn output.
}





