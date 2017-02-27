## makeCacheMatrix sets the matrix to the incoming square matrix.
## if the matrix is not square, the program will exit

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
 if (nrow(x) != ncol(x)) {  
   message("only square matricies can be solved")
   return 
   }
  ## can set the matrix directly to the makeCacheMatrix Vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## read directly from teh makeCacheMatrix Vector
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse
       )
}

## taking an input of a makeCacheMatrix object, this will calculate the 
## inverse and return it after storing in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## after checking if there is already a solution in the cache
  m <- x$getInverse()
  if(!is.null(m)) {message("getting cached data")
    return(m)}
  data <- x$get()
  m<-solve(data)
  x$setInverse(m)
  m
}
