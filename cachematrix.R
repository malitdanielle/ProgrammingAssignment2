## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

## PeerAssesment R Programming, Data Science Specialization
## Implementation following the pattern as described in the Assignments instructions
## The Pattern consists of two parts
## 1. The Cache, see function "makeCacheMatrix"
## 2. The Solver which uses the cache, see function "cacheSolve"
##
## This file is organized in 2 sections
## 1. the implementation of the functions
## 2. some little tests that show how the test works.

##########################################################################
## Implementation 
##########################################################################

## Basically a little Cache "object" Constructor. 
## Compare to "makeVector" from instructions
## Little extension is a cache hit counter, which counts
## the number of succesful cache uses
##
## Param: x, optional matrix might be NULL
## Return: list of functions for cache manipulation
##
makeCacheMatrix <- function(x = matrix()) {
	cachedInverseOfMatrix <- NULL; # cache value; see setInverse and getInverse
	cacheHits = 0; # a little counter for cache hits, to see if cache is effective
	##
	##setter. sets the matrix value. each change will also reset the cacheed inverse.
	setMatrix <- function(replacedByThisMatrix=matrix()) {
		x <<- replacedByThisMatrix
		cachedInverseOfMatrix <<- NULL
		cacheHits <<- 0
	}
	##
	##getter. returns the current matrix value
	getMatrix <- function() { 
		x
	}
	##
	##setter. sets the cached inverse computed by the caller
	setInverse <- function(inverse) {
		cachedInverseOfMatrix <<- inverse
		cacheHits <<- 0
	}
	##
	##getter. get current cached inverse of matrix
	getInverse <- function() {
		if(!is.null(cachedInverseOfMatrix)) {
			cacheHits <<- cacheHits + 1
		}
		cachedInverseOfMatrix
	}
	##
	##getter. return the cache hits for the current inverse
	getCacheHits <- function() {
		cacheHits
	}
	##
	##result list with all the function pointers
	##to manage the value and the cached inverse
	list(	setMatrix = setMatrix, 
			getMatrix = getMatrix,
			setInverse = setInverse,
			getInverse = getInverse,
			getCacheHits = getCacheHits)
}


## Solver Function which uses a "object" returnd by makeCacheMatrix
## Compare to "cachemean" from instructions
## Little extension is a cache hit counter, which counts
## the number of succesful cache uses.
##
## Param: x, makeCacheMatrix return value, MUST NOT be null
## Return: result of solve function, probably cached
##
cacheSolve <- function(x, ...) {
        #
		#cache access. if it has already been set, use the
		#cached value.
		cachedInverse <- x$getInverse()
		if(!is.null(cachedInverse)) {
			message("getting cached data")
			return(cachedInverse)
		}
		#
		#uncached computation of inverse 
		#and set the cache 
		data <- x$getMatrix()
		computedInverse <- solve(data, ...)
		x$setInverse(computedInverse)
		computedInverse
}
##

##########################################################################
## Test
##########################################################################
##
## PREPARE ##########################
##provide a 3x3 matrix
m <- matrix(c(1,2,3,6,0,4,7,8,9),3,3)
#
#create a "cache"
cache <- makeCacheMatrix()
#
#set the matrix value of the cache
cache$setMatrix(m)
##
## CROSSCHECK #######################
##
#crosscheck 1: same matrix in cache than m?
mFromCache<-cache$getMatrix()
if(!identical(m, mFromCache)) {
	stop("Matrix is not the same")
}
#
#crosscheck 2: at this point the cached inverse must be null
iCache<-cache$getInvers()
if(!is.null(iCache)) {
	stop("Inverse must be null")
}
##
## SOLVE ###########################
##
#now solve first time
s1 <- cacheSolve(cache)
#
#solve second time
s2 <- cacheSolve(cache)
#
#check: s1 and s2 should be identical
if(!identical(s1,s2)) {
	stop("Both inverse computations must be the same")
}
#
#now cacheHits should be 1
hits <- cache$getCacheHits()
if(1!=hits){
	stop("hits should be 1")
}
#
#solve third time
s3 <- cacheSolve(cache)
#
#now cacheHits should be 1
hits <- cache$getCacheHits()
hits
if(2!=hits){
	stop("hits should be 2")
}
