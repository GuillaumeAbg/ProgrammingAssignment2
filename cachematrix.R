## These functions aim to compute the inverse of a matrix
## in a cache in order to facilitate the computation

## This firt function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This second function calculates the inverse of the matrix
## only if the inverse has not been calculated before
## It the inverse already exists, the function just retreive the data

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}



