## The intent of these functions are to cache a matrix and it's inverse at invocation such that
## the inverse will be cached for re-use later. This is especially useful if the inverse of a 
## matrix is very likely to be used.

## The below function sets the matrix and uses the solve function to inverse the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  sImatrix<-function(solve) m<<- solve
  gImatrix<-function() m
  list(set=set, get=get,
       sImatrix = sImatrix,
       gImatrix = gImatrix)
}



## This function computes the inverse of the matrix. If the inverse has already been calculated
## and the matrix is unchanged, it should be retrieved.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$gImatrix()
  if(!is.null(m)){
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$sImatrix(m)
  m
}


       


