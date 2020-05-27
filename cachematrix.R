## The function is to cache the inverse of a matrix.
## functions do

## this function create a matrix cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y){
    y<<-x
    i<<-NULL
  }
  get<-function()x
  set_inverse<-function(inverse) i<<-inverse
  get_inverse<-function() i
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)

}


## this function compute the inverse of matrix returned above.
## If the inverse has already been calculated (and the matrix has 
## not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i<-x$get_inverse
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i<-solve(data,...)
  x$set_inverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
