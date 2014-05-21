##############################################################################
## 
##	R Programming Assignment 2
##	Matt Behrens
## 
##
## 	This function creates a special matrix that can cache its inverse.
##
##############################################################################
makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setInv<-function(Inv) i <<-Inv
	getInv<-function() i
	list(set=set,get=get,setInv=setInv,getInv=getInv)

}


##
## This function computes the inverse of the special matrix returned 
##     from makeCacheMatrix function.
##
cacheSolve <- function(x, ...) {
	i<-x$getInv()
	if(!is.null(i)){
		message("Retrieving Cached Data.....")
		return(i)
	}
	out<-x$get()
	i<-solve(out,...)
	x$setInv(i)

	i

}

##############################################################################
##
##	Test Case from forums
##
##
##amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##amatrix$get()         # Returns original matrix
##cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
##amatrix$getinverse()  # Returns matrix inverse
##cacheSolve(amatrix)   # Returns cached matrix inverse using previously 
##				# computed matrix inverse
##
##amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
##cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
##amatrix$get()         # Returns matrix
##amatrix$getinverse()  # Returns matrix inverse
##
##
##############################################################################
##############################################################################
##
##	Example from assignment listed below
##
##
##makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}
##
##
##cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}
##
##############################################################################