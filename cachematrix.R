## This pair of functions calculate the inverse of a given matrix 
## but first they check if that value has been already calculated and stored

## This first function initially sets the cache and then stores the values 
## of the inverse matrix in an objectonce it's been calculated

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function checks if the inverse matrix we are looking for is cached.
## If it's cached it returns it. If it's not, it calculates it using the solve
## function and stores it with the help of the first function

cacheSolve <- function(mcm, ...) {
  m2 <-mcm$getinv()
  if(!is.null(m2)) {
    message("getting cached data")
    return(m2)
  }
  data<-mcm$get()
  m3<-solve(data,...)
  mcm$setinv(m3)
  m3
  
}
