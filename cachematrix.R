## Put comments here that give an overall description of what your
## functions do


##  The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the input matrix
## 4.get the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse_) m<<- inverse_
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if (!is.null(m)){
          message("getting cached inverse matrix!")
          
          return (m)
    }
    data<-x$get()
    m<-solve(data)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
