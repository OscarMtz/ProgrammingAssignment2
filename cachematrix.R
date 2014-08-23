## Aug-21-2014 Initial Creation by Oscar Portillo

## Function makeCacheMatrix creates a special matrix object that can cache its inverse and function
##chaceSolve computes the inverse of the special function returned by makeCacheMatrix

## makeCacheMatrix function creates a special matrix object which is a list of set/get
## the value of the matrix as well as set/get its inverse 

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


## The function cacheSolve checks if the inverse has already being calculated. 
## If so, it retrieves the inverted matrix from the cache instead of recomputing, 
## otherwise it computes the inverse matrix, and sets it in the cache via the setInverseMatrix function.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m<-x$getInverseMatrix()
  
  ## Recall that is.null returns TRUE if its argument is NULL and FALSE otherwise.
  ## So if the inverse matrix was previously computed, it is returned from cache data
  if(!is.null(m))
  {
    message("getting cache data")
    return (m)  ## last dataline evaluated if is.null(m) returns FALSE              
  }
  
  ## If null object is TRUE, the inverse matrix is computed via funcion "solve"
  
  matrix <- x$get()
  
  m<- solve( matrix, ...)
  
  x$setInverseMatrix(m)
  
  m   ## Inverse matrix is returned
}
##  Function Validation: The above functions were tested using the 3x3 matrix 
##  described http://www.purplemath.com/modules/mtrxinvr2.htm
##  The following lines show console outputs. It can be seen that the inverse
##  matrix is calculated correctly.
##
##  > a<- c(1,0,5)
##  > b<-c(2,1,6)
##  > c<-c(3,4,0)
##  > d<-cbind(a,b,c)
##  > d
##  a b c
##  [1,] 1 2 3
##  [2,] 0 1 4
##  [3,] 5 6 0
##
##  > z<-makeCacheMatrix()
##  > z$set(d)
##  > cacheSolve(z)
##  [,1] [,2] [,3]
##  a  -24   18    5
##  b   20  -15   -4
##  c   -5    4    1
##
##  > cacheSolve(z)
##  getting cache data
##  [,1] [,2] [,3]
##  a  -24   18    5
##  b   20  -15   -4
##  c   -5    4    1

