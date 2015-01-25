## These functions in conjunction enable the user to both compute and cache calculations in matrix inverses. This is done with a goal of performance, 
##so that answers can be saved in the event that matrices need to be inversed multiple times.

## This function creates a list of four functions, which enable the user to save matrix inversions and recall them. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) { #saves matrix 
    x <<- y
    m <<- NULL #indicates there is no cached ans
  }
  get <- function() x #return saved matrix
  setInv <- function(inv) m <<- inv #caches inverse
  getInv <- function() m #returns cached inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function interacts with the above function to either pull the cached matrix inverse, or calculates the inverse, saves it, and returns it to the user. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() #value of m is null (no cached ans) or the inverse
  if(!is.null(m)) {  #if there is cached, this thread is activated and cached ans returned
    message("getting cached data")
    return(m)
  }
  data <- x$get() #save matrix
  m <- solve(data, ...) #solve matrix inverse
  x$setInv(m) #store ans in cache
  m #return ans
}
