## In the code we have 2 functions, "cacheSolve" is used to calculate 
## the inverse of a matrix, and "makeCacheMatrix" is used to save
## the precalculated inverse to do it only once

## This function contain multiple functions used to move data with "sets"
## and "gets" to calculate just once the inverse of a matrix caching the
## value of the solution in mat.inverse
makeCacheMatrix <- function(x = matrix()) {
  # Variable to save the inverse matrix for caching
  mat.inverse<-NULL
  # Set the raw matrix and assign NULL to the inverse
  set<-function(y){
    x<<-y
    mat.inverse<<-NULL
  }
  # Get the raw matrix
  get<-function() x
  # Set the calculated inverse to mat.inverse
  setinverse<-function(y) mat.inverse<<-y
  # Get the precalculated inverse
  getinverse<-function() mat.inverse
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function receive a parameter x that is a reference to function
## "makeCacheMatrix" and uses its functions to get the raw matrix
## to compute the inverse and save it in x, and it checks everytime
## if the inverse is already calculated to return directly the value
cacheSolve <- function(x, ...) {
  # Retrieve the current inverse value
  mat.inverse<-x$getinverse()
  # If the inverse value is not null
  # it is returned
  if(!is.null(mat.inverse)){
    message("getting cached data")
    return(mat.inverse)
  }
  # If the inverse is not calculated
  # the raw matrix is getted
  mat.raw<-x$get()
  # The raw matrix is solved and
  # saved in mat.inverse
  mat.inverse<-solve(mat.raw)
  # The inverse is saved in x
  x$setinverse(mat.inverse)
  # The solution is returned
  mat.inverse
}

## TESTED with the following code:
##
## source("cachematrix.R")
## X<-matrix(5,1000,1000)
## for (i in 1:1000) X[i,i]<-1
## M<-makeCacheMatrix(X)
## system.time(cacheSolve(M))
#### user   system  elapsed
#### 0.852  0.012   0.866
## system.time(cacheSolve(M))
#### getting cached data
#### user   system  elapsed
#### 0      0       0

