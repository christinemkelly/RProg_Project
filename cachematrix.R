## The purpose of this function is to cache the inverse of a matrix so that it isn't 
##calculated each time, which can take too much time. The learning objective of this
##assignement is to see how two different operators, <- and <<- affects the variables. 
##The assignement addresses lexical scoping as well as coding.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##The matrix inverse is found using the R solve function.

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
##This function computes the inverse of the matrix created above.
##It first checks to see if the inverse has been calcualted previously and if so, gets the
##inverse from the cache and skips teh rest of the function. If the inverse has not been
##cached, the inverse will be computed and cached for future use.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setmatrix(m)
    m
}
