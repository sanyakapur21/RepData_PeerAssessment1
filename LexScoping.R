makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  setmat<- function(y){
    x<<-y
    m<<-NULL
  }
  getmat<- function()x
  setInv<- function(inverse) m<<- inverse
  getInv<- function() m
  list(setmat = setmat, getmat=getmat, 
       setInv=setInv, 
       getInv=getInv)
}


cacheSolve <- function(x, ...) {
  m<- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat<- x$getmat()
  m<- solve(mat,...)
  x$setInv(m)
  m
}