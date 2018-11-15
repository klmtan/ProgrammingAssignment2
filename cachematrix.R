## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###Created by Katherine Tan
###As requirement for R Programming Assignment 2
###Example to run
###Load the file in R studio
###source("cachematrix.R")
###CREATE A MATRIX 
### a <- matrix(c(1,2,2,3,4,5,6,7,8),nrow=3,ncol=3)
### RUN THE FUNCTION makeCacheMatrix
### b <- makeCacheMatrix(a)
### RUN THE FUNCTION cachesolve
### c <- cachesolve(b)
### c

#this makes the functions to be used for the 
#matrix
#setter for the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y   						#assigns the matrix value given by the user
		m <<- NULL						#makes an variable to hold the inverse value
	}
	get <- function() x					#returns the matrix value
	set_inverse <- function(solve) m <<- solve 		#sets the value of the solved inverse to var m
	get_inverse <- function() m 		# gets the inverse of m
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) #returns the list of functions for cached matrix
}


## Write a short comment describing this function

#this function takes parameters of type makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()		#gets the inverse of the matrix
        if(!is.null(inverse)){			#checks if it is not null it means that the inverse is already computed
        	message("getting cached data")
        	return(inverse)
        }
        data <- x$get()					#if the value is null. Then the matrix in stored in the makecachematrix will be fetched
        inverse <- solve(data,...)		#the inverse of the matrix is computed using the solve function
        x$set_inverse(inverse)			#the value of the computed inverse will be set to the var x 
        inverse 						#returns the value
}
