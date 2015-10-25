
## ASSIGNMENT: MakeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.

##makecacheMatrix is a function that has nested functions get, SetInv, and getInv
##There is an error check to make sure that the matrix is a square matrix, as we cannot
##find the inverse of a non-square matrix. It also checks to make sure determinant is non 0.
## get function returns the original matrix passed to makeCacheMatrix
##setInv is a setter which sets a variable "InvMatrix" to the inverted matrix which is passed
##getInv is a getter which gets the inverted Matrix that was stored in InvMatrix
makeCacheMatrix <- function(OriginalMatrix = numeric())
{
  if(!nrow(OriginalMatrix) == ncol(OriginalMatrix))
  {
    print("Not a square matrix")
  }
  if(det(OriginalMatrix) == 0)
  {
    print("Cannot find inverse")
  }
  InvMatrix <- NULL
  get <- function()
  {
    OriginalMatrix
  }
  setInv <- function(Inverted)
  {
    InvMatrix <<- Inverted
  }
  getInv <- function()
  {
    InvMatrix
  }
  list( get = get,
       setInv = setInv,
       getInv = getInv)
}


## ASSIGNMENT: cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

## cacheSolve first gets the inverse if by calling the getter in object X.
## if getter returns a non-null matrix, then we know it is cached and it returns InvMatrix
##if it is null, then we know that it is not cached, and therefore need to compute the inverted matrix
## and then call the setter to set the inverted matrix.
cacheSolve <- function(x, ...) {
  InvMatrix <- x$getInv()
  if(!is.null(InvMatrix)) 
  {
    print("getting cached data")
    return(InvMatrix)
  }
  else
  {
    data <- x$get()
    Inverted <- solve(data, ...)
    x$setInv(Inverted)
    Inverted
  }
}