
## FUNCTION NAMES ARE: setmx, getmx; getinv, setinv 
## Following the blueprint code of MakeVector, the following function
## allows the user to store and retrieve the value of a matrix and its inverse
   ## Default matrix taken from sample matrix that in the Coursera forum

makeCacheMatrix <- function(mxnow = matrix(c(1/2, -1/4, -1, 3/4), 2, 2)) {
  inv <- NULL
  
  setmx <- function (newmx){
    mxnow <<- newmx
    inv <<- NULL
  ## setmx takes a matrix argument and sets it as the new matrix, erasing the cached
  ## inverse of the previous matrix
  } 

  getmx <- function(){
    mxnow
  }
  ## getmx, when called, simply allows the user to view the currently stored matrix
  
  setinv <- function(newinv){
    inv <<- newinv
  }
  ## A new inverse now exists in the function's environment

  getinv <- function(){
    inv
  }
  
  list(setmx = setmx, getmx = getmx,
       setinv = setinv,
       getinv = getinv)
  ## Function makeCacheMatrix returns a list through which an object can call its functions
  ## by simply using the $ operator ie. newObject$getinv()
}
##___________________________________________________________________________________

## The following function calculates the inverse of the matrix or retrieves the previously
## cached inverse from the makeCacheMatrix object, if present 

cacheSolve <- function(mxobj, ...) {
  inv <- mxobj$getinv()
    ##Load up inverse from makeCacheMatrix object 
  if(!is.null(inv)){
      return(inv)
  }
    ## If returned inverse is not null, ie. already cached, then simply return cached inverse 
  getnewmx <- mxobj$getmx()
    ## If no inverse found, start computing for new inverse by retrieving saved matrix
  inv <- solve(getnewmx, ...)
    ## Compute for inverse of matrix using solve() function
  mxobj$setinv(inv)
    ## Save or cache newly computed matrix inverse
  inv
  }
