## This pair of functions calculate the inverse of a given matrix 
## but first they check that value hasn't been already calculated and stored

## This first function initially sets the cache and then stores the values 
## of the inverse matrix once it's been calculated

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<--y
    m<--NULL
  }
  get<-function() x
  setinv<-function(inv) m<--inv
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function checks if the inverse matrix  we are looking for is cached.
## If it's cached it returns it. If it's not, it calculates it using the solve
## function and stores it with the help of the first function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
