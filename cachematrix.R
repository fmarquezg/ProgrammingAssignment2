## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#MakeCacheMatrix:
#Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  #Create matrix in the WORKING environment
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #Get Value of Matrix
  get<-function() x
  #invert and store matrix
  setInv <- function(inverse) inv <<- inverse
  #Get inv matrix from cache
  getInv<- function() inv
  #Retun list to working env
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function
#Computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("Inverse in Cache!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}

