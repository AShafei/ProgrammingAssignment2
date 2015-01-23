## makeCacheMatrix: is a function provide a facilty to store a matrix and its inverse and can cache 
## the inverse.
## it's provide a set of functions that can set a matrix, get the matrix, set the inverse and get inverse

##cacheSolve: is a function that resolve the inverse of a matrix created by 
##makeCacheMatrix function

##This function is used to create a special matrix and can
##cache its inverse if the matrix is not changed
makeCacheMatrix <- function(x = matrix()) {
	##Hold the invs of a matrix
	invsMat <- NULL

	##set the input matrix
	set <- function(y){
		##check if the previous Stored matrix is identical to the input
		if(!identical(x,y)){
			##set the new matrix and reset the invsMatrix
			x <<- y
			invsMat <<- NULL
		}
	}##end of set function

	##get the stored matrix
	get <- function() x

	##set the invs of matrix
	setInvs <- function(invs) invsMat <<-invs

	##get the cached invs
	getInvs <- function() invsMat

	##list the functions
	list(set = set, get = get,
             setInvs= setInvs,
             getInvs= getInvs)

}##end of makeCacheMatrix function


## Return a matrix that is the inverse of 'x' using caching mechanism
cacheSolve <- function(x, ...) {
	##Check if the inverse is cached [Already calcuated before]
	invsMat <- x$getInvs()
	if(!is.null(invsMat)) {
		message("getting cached inverse")
		return(invsMat)
	}
	data    <- x$get()
	##calculate the inverse
	invsMat <- solve(data, ...)
	x$setInvs(invsMat)
	invsMat
}##end of cacheSolve function
