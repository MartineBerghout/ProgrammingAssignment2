## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function creates a matrix to be used by the cacheSolve function hereafter 
##in order to calculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) { ##defining matrix function called makeCacheMatrix and open the loop by using {
    m<-NULL ##making sure variable m is empty, avoiding m to have been populated during a previous run
    set<-function(y){ ##define set as a function of y and open a loop using { as I only want the following two lines to be applied to 'm' and 'set'
      x<<-y ##x=y; y is an out of the loop variable
      m<<-NULL ##m=empty (NULL) withing this loop
    } ##end of loop
    get<-function()x ##defining get as a function of x
    setInverse<-function(inverse)m<<-inverse ##defining to set the inverse function of m
    getInverse<-function()m ##defining to get the inverse of m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) ##list of self reference
  } ##end of loop

## Write a short comment describing this function 
##cacheSolve will return the inverse of the matrix defined in the makeCacheMatrix function. 
##Detailed comments after each line.

cacheSolve <- function(x, ...) {##return a matrix that is the inverse of 'x'
  m<-x$getInverse() ##m is the getInverse of x (as defined in makeCacheMatrix)
  if(!is.null(m)){ ##condition to check if m has been calculated before and open a loop for this condition by using {
    message("getting cached data") ##if true, then get message "getting cached data" which indicates previously calculated data
    return(m) ##returning the previously calculated data
  } ##end this loop
  data<-x$get() ##if the 'if' condition has not been met, then get the data for x in the makeCacheMatrix function
  m<-solve(data,...) ##m now needs to solve the matrix data (by reading the whole matrix)
  x$setInverse(m) ##now it calculates the inverse of m as defined in the makeCacheMatrix function
  m ##print value of m
}##end of loop

