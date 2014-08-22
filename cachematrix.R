## Aug-21-2014 Initial Creation by Oscar Portillo

## Function makeCacheMatrix creates a special matrix object that can cache its inverse and function
##chaceSolve computes the inverse of the special function returned by makeCacheMatrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  set<- function(y){
  
    x<<- y
    
    m<<- NULL
  }
  
  get<- function()x
  
  setInverseMatrix<- function(solve) m <<- solve
  
  getInverseMatrix <- function()m

  list( set = set, get = get, 
        
        setInverseMatrix = setInverseMatrix, 
        
        getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverseMatrix()
  if(!is.null(m))
  {
    message("getting cache data")
    return (m)
  }
  
  matrix <- x$get()
  
  m<- solve( matrix, ...)
  
  x$setInverseMatrix(m)
  
  m
}
