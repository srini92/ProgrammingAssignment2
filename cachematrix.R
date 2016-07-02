## The two functions mentioned below are used to create a special object that stores a matrix and caches its inverse


## This function is used to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invers<-NULL
set<-function(y)
{
x<<-y
invers<<-NULL
}
get<-function()x
setInverse<-function(inverse) invers<<-inverse
getInverse<-function()invers
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function is used to compute the inverse of the special matrix created by makeCacheMatrix.
## If inverse is calculated before then it retrieves the inverse from the cache if the matrix is not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invers<-x$getInverse()
if(!is.null(invers))
{
message("getting cached data")
return(invers)
}
data<-x$get()
invers<-solve(data, ...)
x$setInverse(invers)
invers
}
