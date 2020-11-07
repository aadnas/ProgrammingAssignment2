## The goal of the functions is to inverse an invertible matrix, the first function creates a list of the supplied matrix and a place to cache the inverse matrix, and the second one checks if the matrix inverse is in the cache and if it is not calculates the inverse and stores it to the cache.


# The function starts with writing NULL to matrx, to make sure it is empty. Then with set() it writes the input to x and again sets NULL to matrx, in the parent environment. get() retrives x from the parent environment. setinverse() sets what to solve. Last, getinverse() gets what is solved. Then all is put in to a list.
makeCacheMatrix <- function(x = matrix()) {
  matrx <- NULL
  set <- function(y){
    x<<-y
    matrx<<-NULL
  }
  get<-function() x
  setinverse <- function(solve) matrx<<-solve
  getinverse <- function() matrx
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## First it retrieves from getinverse, and the checks if it is NULL, if it is not NULL it reads getinverse() and lets us known that is has been calculated and writes what is in the cache. If it is NULL then the inverse matrix is calculated

cacheSolve <- function(x, ...) {
  matrx<-x$getinverse()
  if(!is.null(matrx)){
    message("reading cached data")
    return(matrx)
  }
  data<-x$get()
  matrx<-solve(data,...)
  x$setinverse(matrx)
  matrx
        ## Return a matrix that is the inverse of 'x'
}