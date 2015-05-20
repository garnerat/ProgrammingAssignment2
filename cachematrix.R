## Together makeCacheMatrix and cacheSolve calculate the inverse of a matrix and
## will store the result for future calls of cacheSolve as long as the matrix 
## remains the same

## Input method 
## p <- matrix
## q <- makeCacheMatrix(p)
## cacheSolve(q)

## makeCacheMatrix - initializes variable m as NULL, recieves a matrix
## as argument x, and creates a list containing four functions...

## set - receive a matrix argument y to independently update matrix arg x
## using the <<- operator, resets m varaible to null

## get - returns the matrix given by the argument to makeCacheMatrix or set functions

## setinv - stores/overwrites the cached value of the matrix inverse from input from
## computation in cacheSolve function

## getinv - returns the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve checks for cached inverse matrix. If the cache is not null, the function
## returns stored value from previous cacheSolve. If the cache value from geting()
## is NULL then calculation of the matrix inverse occurs and is passed to the 
## setinv() function to cache the new result before returning the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
