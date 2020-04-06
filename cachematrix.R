# creates a makeCacheMatrix object, matrix and inverse variables with a list 
# of four functions: 
# set() which assigns a new value to the matrix using <<- and clears inv; 
# get() which accesses the matrix; 
# setinv() which assigns a new inverse value; and 
# getinv() which accesses the current inverse value (inv)
makeCacheMatrix <- function(curMatrix = matrix(numeric(), nrow = 2, ncol = 2)) { 
        # upon creation of class object, initialize inverse of curMatrix to NULL
        curInv <- NULL
        # mutator method which assigns newMatrix to curMatrix and clears curInv
        # (using superassignment operator for both to access in parent 
        # environment)
        set <- function(newMatrix) {
                curMatrix <<- newMatrix
                curInv <<- NULL
        }
        # accessor method which returns the current value contained in curMatrix
        get <- function() curMatrix
        # mutator method which assigns a new inverse to curInv 
        # (using superassignment operator to access in parent environment)
        setinv <- function(newInv) curInv <<- newInv
        # accessor method which returns the current value contained in curInv
        getinv <- function() curInv
        # return a list with memory locations in parent environment for each 
        # method
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# takes the passed makeCacheMatrix object and uses it to call functions 
# involved in returning the inverse variable (curInv) corresponding to the 
# matrix (curMatrix) variable. If it has already been calculated, it returns
# that value; otherwise, it calculates it from scratch by calling solve()
cacheSolve <- function(mkCacheMtxObj, ...) {
        # attempt to retrieve use passed object's curInv and assign to local inv
        inv <- mkCacheMtxObj$getinv()
        # inverse is non-null, already exist in cache, so return cached inv 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculate inverse anew by accessing passed object's 
        # curMatrix
        data = mkCacheMtxObj$get()
        # create identity matrix of matching size to curMatrix
        ident = diag(dim(data)[1])
        # call solve, passing curMatrix and commensurate identity matrix
        inv <- solve(data, ident, ...)
        # update curInv by using passed object to call mutator method setinv()  
        mkCacheMtxObj$setinv(inv)
        # return inverse
        inv
}
