
#R Assignement for Papiya Paul (papiya.paul@tcs.com)

# This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix() function holds getter setter method for input matrix and its inverse

makeCacheMatrix <- function(x=matrix()) {

    inv_matrix <- NULL  # stores the inverse matrix

    set <- function(y) {  # setter method of input matrix
	x <<-y
	inv_matrix <<- NUL
    }

    get <- function() x  # getter method of input matrix

    setInverse <- function(inverse) inv_matrix <<- inverse  # setter method of inverse matrix

    getInverse <- function() inv_matrix  # getter method of inverse matrix

    list(set = set,
	 get = get,
	 setInverse = setInverse,
	 getInverse = getInverse)
}


#  This function take a matrix as input
#  check into the cache if it is already calculated
#  if not it generate the inverse matrix
#  return inverse matrix

cacheSolve <- function(x,...){

   inv_matrix <- x$getInverse() # get the matrix inverse

   if(!is.null(inv_matrix)) { # check if inverse matrix already present in cache
      message("inverse matrix fetched from cache")
      return(inv_matrix)
   }

   # if inverse matrix is not present in cache
   matrix <- x$get()  # get input matrix 
   message("inverse matrix getting calculated")
   inv_matrix <- solve(matrix,...)   # calculate inverse matrix
   x$setInverse(inv_matrix) # set inverse matrix
   inv_matrix  # return inverse matrix

} 
