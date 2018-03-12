## This function creates a special 'matrix'
##It sets the value of a matrix, then gets the value, then sets 
##value of the inverse using solve(), and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set<-function(y){
      x<<-y
      inv <<-NULL
   }
    get<-function() x
    setInverse<-function() inv <<- solve(x)
    getInverse<-function() inv
    list(set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## The function below gets the value of the inverse of the matrix created above
##by checking first if the value is cached, and then returning the cached value
##if not, it calculates the inverse and caches the value using setInverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix<-x$get()
    inv <-solve(matrix, ...)
    x$setInverse(inv)
    inv
}
       
