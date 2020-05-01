## Put comments here that give an overall description of what your
## functions do
## the two functions below create a matrix object and a function that gets its cached inverse
## if the object does not have a cached inverse, the second function computes and sets it


## Write a short comment describing this function
## create the object with inverse object and the respective gets and sets

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL #sets inverse object to NULL
  set <- function(y) {
    x <<- y #sets matrix x to passed y
    mx <<- NULL #matrix changed therefore set inverse object to NULL
  }
  get <- function() x #returns the matrix
  
  setmx <- function(solve) mx <<- solve #sets the inverse object to  passed solve

  getmx <- function() mx  #returns the inverse variable (which can be NULL)
  
  list(set=set, get=get,
       setmx = setmx,
       getmx = getmx) #return list of functions
  
  
  
}


## Write a short comment describing this function
## get the cached inverse, if there is no cache (i.e. returned NULL),
## get the matrix, compute its inverse, set mx in matrix (i.e. cache it), and return mx

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getmx() #calls function that gets inverse object
  if(!is.null(mx)) { #if the inverse object is not null then value exists, return inverse object mx
    message("getting cached data")
    return(mx)
  }
  #if the code below executes, this means mx is NULL (returned NULL)
  #so we need to compute it ourselves
  data <- x$get() #to do so, we get the matrix 
  mx <- solve(data,...) #we apply solve to get the inverse
  x$setmx(mx) #we set the inverse using setmx, we do this in order to cache the inverse, 
  #next time we call x$getmx(), it will return the inverse and we won't need to compute it
  
  mx #return the inverse
}
