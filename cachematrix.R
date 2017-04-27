## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
      #This function creates a list of functions which set matrix,get the matrix,
      #set the inverse matrix and gets the inverse matrix for the given matrix
      
      
      im <- NULL #im is for inverse matrix
      
      set <- function(y)
      {
            x <<- y
            im <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(inv) im <<- inv
      
      getinv <- function() im
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      im <- x$getinv()
      
      if(!is.null(im))
      {
            message("getting cached inverse matrix")
            return(im)
      }
      
      ## else part is below:
      
      message("This is the first time inverse of this matrix requested hence no cached entry.")
      data <- x$get()
      im <- solve(data)
      x$setinv(im)
      
      im
}
