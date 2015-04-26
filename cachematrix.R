
## These two functions create a special object that stores a matrix and
## cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix",
## which is a function that does the following:
## 1)set the value of the matrix (set_matrix)
## 2)get the value of the matrix (get_matrix)
## 3)set the value of the inverse (set_inverse)
## 4)get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 					## Initially assigns 'NULL' to inverse
    set_matrix <- function(y) {
        x <<- y 					## Sets the matrix 'x'
        inverse <<- NULL
    }
    get_matrix <- function() x 				## Returning matrix 'x'
    set_inverse <- function(solve) inverse <<- solve 	## Cache the value of the inverse
    get_inverse <- function() inverse 			## Returns the inverse
    list(set_matrix = set_matrix, get_matrix = get_matrix,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## Bit first it checks to see if the inverse has already been calculated.
## If it has, it gets the inverse from the cache and skips the calculation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via
## the 'set_inverse' function.

cacheSolve <- function(x, ...) {				## Returns a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()				## Gets the inverse
    if(!is.null(inverse)) {					## Checks for the presence of inverse
        message("getting cached data")			## Displays message
        return(inverse)
    }
    data <- x$get_matrix()					## Gets Matrix
    inverse <- solve(data, ...)				## Uses solve() to compute inverse
    x$set_inverse(inverse)					## Cashes the inverse
    inverse 						## Returns the inverse
}