#This assignment is to compute the Matrix Inversion. We know that Matric Inversion can take in quite some space
#in the memory, let's cache the inverse of the matrix to help us save some computation cost. We are going to create
#functions that would cache the matrix inverse

#makeCasheMatrix() function creates a special "matrix" object that can cache its inverse.
#The returned special "matrix" is basically a list containing functions to :
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  #set matrix
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  #get matrix
  get <- function() 
    x
  #set inverse matrix
  setinv <- function(inverse) 
    i <<- inverse
  #get inverse matrix
  getinv <- function() 
    i
  #r special "matrix" list
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}

#cacheSolve() function computes the inverse of the special "matrix". 
#So initially, it checks whether the inverse has already been computed.
#If the inverse has already been calculated and the matrix has not changed, then it retrieves the inverse from cache.
#Otherwise, it calculates the inverse and sets it in the cache.
#Finally, returns the inversed matrix.

cacheSolve <- function(x, ...) 
{
  i <- x$getinv()
  #Check whether the inverse is available in cache
  if (!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}