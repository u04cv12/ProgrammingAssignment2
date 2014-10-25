## Caching the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinv<-function(ginv)i<<-ginv
  getinv<-function()i
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve<-function(x,...){
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-ginv(data,...)
  x$setinv(i)
  i
}
