## makeCacheMatrix function stores the matrix in global variable "m" and the inverse of matrix in global variable "m_inv". 
## The function returns a list that contains the get and set funtions to fetch and store the 2 variables respectively.

makeCacheMatrix <- function() {
        m <<- NULL
        m_inv <<- NULL
        set <- function(y) {
                m <<- y
                m_inv <<- NULL
        }
        get <- function() m
        setinverse <- function(inv) m_inv <<- inv
        getinverse <- function() m_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve funtion takes a list and a matrix as arguments. It fetches the matrix and it's inverse from the matrix list.
## It then checks, if the matrix argument is same as that stored in matrix list and whether the inverse value is already computed for the same.
## If the above condition pass, it returns the stored value. Else it calculates, stores & returns the inverse of matrix argument.

cacheSolve <- function(mtxlist, mtx) {
        data <- mtxlist$get()
        data_inv <- mtxlist$getinverse()
        if(!is.null(data_inv) & identical(data,mtx)) {
                message("getting cached data")
                return(data_inv)
        }
        data_inv <- solve(mtx)
        mtxlist$set(mtx)
        mtxlist$setinverse(data_inv)
        data_inv
}
