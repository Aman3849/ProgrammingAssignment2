#This set of functions calculates the inverse of a matrix and stores it as cache, 
#so that we can easily retrieve the inverse next time we need it. 

#This function is used to create the cahche of matrix.
makeCacheMatrix <- function(x = matrix()) {
#We initialize the matrix value as NULL, is case previous data was stored in the variable.
    m<-NULL
    set<- function(y){
#Set function changes the value of x in case matrix is changed. 
        x<<-y
        m<<-NULL
    }
#The function retrieves the vale of x.
    get<-function()x
#the function calculates the inverse of the inverse of the matrix and stores it as 'm'.
    setinv<- function(solve) m<<-solve
#the function getinv retrieves the value of inverse stoored it in m.
    getinv<-function() m
#returns a list consisting of four functiins
    list(set = set,get = get,setinv = setinv, getinv= getinv)

}


#This function looks for stored caches, and returns the value and if it is not there it finds out the inverse before cacheing it.

cacheSolve <- function(x, ...) {
#looks for cached inverse
    m<-x$getinv()
#if inverse is there, prints out the cached value
    if(!is.null(m)){
        message("Getting Cached Matrix Inverse")
        return(m)
    }
#if inverse of matrix isn't cached already, it find outs the inverse and cache it.
    mat<-x$get()
    m<-solve(mat)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}




