## This file contains a pair of functions that cache the
## inverse of a matrix

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <<- NULL
  
  set <- function(y){  #funtion to set value of matrix
    x <<- y
    inverseMatrix <<- NULL #set inverse to Null for a new matrix
  }
  
  #fucntion to set inverseMatrix 
  setinverse <- function(inverse) inverseMatrix <<- inverse
  
  #function to return the original matrix
  get <- function() x
  
  #function to return the inverse matrix
  getinverse <- function() inverseMatrix
  
  list(set = set, setinverse=setinverse, get=get, getinverse=getinverse)
}


## This function computes the inverse of the special 'matrix'
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated & the matrix has
## not changed, then it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix) #if inverse exists, return value from cache
  }
  data <- x$get()
  inverseMatrix <- solve(data) # compute inverse of given matrix
  x$setinverse(inverseMatrix)
  inverseMatrix
  
}
