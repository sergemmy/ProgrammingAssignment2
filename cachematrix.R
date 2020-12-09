## the function creates a special matrix object that can cache the
## inverse.

## first it sets the matrix in cache, gets the value of the matrix, caches the
## the inversion and gets the inversed matrix from cache

makeCacheMatrix <- function(x = matrix()) {
            m<-NULL
            set <- function(y) {
                x<<-y
                m<<-NULL
            }
            get<-function() x
            cacheinv<-function(solve) m<<-solve
            getinv<-function() m
            list(set=set, get=get, cacheinv=cacheinv, getinv=getinv)
  
  
}


## this function computes the inverse of the matrix, created by previous
##function. If the inverse already exists in cache it returns
##the inversed matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()
            if(!is.null(m)) {
              message("getting cashed data")
              return(m)
            }
            data<-x$get()
            m<-solve(data,...)
            x$cacheinv(m)
            m
}
