##Assignment: Caching the Inverse of a Matrix
##Functions that cache the inverse of a matrix.
##1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL
                    set <- function(y) {
                      x <<- y
                      m <<- NULL #Inverse of a Matrix
                    }
                    get <- function() x 
                    setinverse <- function(inverse) m <<- inverse
                    getinverse <- function() m
                    list(set = set, get = get,
                         setinverse =  setinverse ,
                         getinverse =  getinverse )  
}


##2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m #Inverse of a Matrix
}


## Sample run:  
## > x = rbind(c(1, 5),c(2, 1)) 
## > m = makeCacheMatrix(x)
## > m$get()
##        [,1] [,2]
## [1,]    1    5
## [2,]    2    1
## First Time of run (No Cache)
## > cacheSolve(m)
##         [,1]       [,2]
## [1,] -0.1111111  0.5555556
## [2,]  0.2222222 -0.1111111
## After Firts run (With Cache Data)
## > cacheSolve(m)
## getting cached data
##          [,1]       [,2]
## [1,] -0.1111111  0.5555556
## [2,]  0.2222222 -0.1111111
##
