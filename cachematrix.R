## Caching the Inverse of a Matrix
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inver<-NULL
	set<-function(y){
		x<<-y
		inver<<-NULL
	}
	
	get<-function() x
	setinv<-function(inv) inver<<-inv
	getinv<-function() inver
	
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inver<-x$getinv()
	if(!is.null(inver)){
		message("getting cached inverse")
		return(inver)
	}
	
	data<-x$get()
	inver<-solve(data)
	x$setinv(inver)
	inver
}
