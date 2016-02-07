## two functions that togeter cashe and inverse a matrix to avoid 
##  costly unnessecary computing if the inverse exist


## Create a special matrix and cashe the inverse

makeCacheMatrix <- function(xm = matrix()) {
      inv<- NULL 
      set<-function(x) {
            xm<<- x
            inv<<- NULL
      }
      get<- function() xm
      setinv<- function(inv) inverse<<- inv
      getinv<- function() inverse
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function use the spesial matrix created above as an argument
## and returns the cached inverse matrix if it exist, or compute the inverse 
## if needed and saves it

cacheSolve <- function(xm, ...) {
      inv<- xm$getinv()
      if(!is.null(inv))
            inv
      inv<- solve(xm$get(), ...)
      xm$setinv(inv)
      inv
}

