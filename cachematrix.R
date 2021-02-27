## Put comments here that give an overall description of what your
## functions do

## Adaptation of the Make Vector function, who returns a list of the inner functions who are asignes
## to another environment by lexical scoping, set function uses this to asign the values
## to the extra environment and let us not use the asigned values in that environment in the global one

makeCacheMatrix <- function(x = matrix()) {
  
  r <- NULL
  

  set <- function(y){
    x <<- y
    r <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) r <<- inverse
  
  getInverse <- function() r 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
  
}


## Returns the cached value of X, checking if is not null and returning
## the inverse using the "getInverse" function, making use of the asigned values to solve the matrix
## A especial condition for singular matrices is added

cacheSolve <- function(x = m(), ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getInverse()
  
  if(!is.null(r)){
    message("getting cached data")
    return(r)
  }
  
  mat <- x$get()
  r <- solve(mat,...)
  if(!is.null(r)){
    message("getting cached data")
    return(r)
  } else {
    message("The Matrix is Singular")
  }
  x$setInverse(r)
  r
}


#Testing:

x <- matrix(c(8, 4, 3, 6, 4, 4, 12, 4, 3),   nrow = 3, ncol = 3)

x_n <- makeCacheMatrix(x)


cacheSolve(x_n)








