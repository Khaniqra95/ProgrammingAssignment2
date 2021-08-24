## Put comments here that give an overall description of what your
## functions do

## There are two functions names makeCachematrix and cacheSolve 
## for caching the inverse of square matrix

## Write a short comment describing this function

## makeCacheMatrix consist of set, get, setInverse, getInverse
## to get a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    ##initialising inverse matrix as null
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get<- function(){x}          ## function to get matrix x
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function(){inv}
  list(set= set, get = get, 
       setInverse = setInverse, 
       getInverse= getInverse)
}



## Write a short comment describing this function
## This is to get the Cache data

cacheSolve <- function(x, ...) ##gets cache data
  {
  inv<-x$getInverse()
  if(!is.null(inv))     ##checking whether inverse in null
    {
    message("getting cache data")
    return(inv)     ##returns inverse 
  }
  mat <-x$get()
  inv<-solve(mat, ...)    ##calculate the inverse
  x$setInverse(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
