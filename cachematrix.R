## The functions create the inverse matrix 
## from the MASS library

## makeChaheMatrix consists of of set, get, setinv, getinv fuctions

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL # sets inverse as NULL 
  set <- function(y){
                    x<<-y
                    inv<<-NULL
                    }
  get<-function()x    #this is how to get matrix X
  setinv <- function(inverse)inv<<-inverse
  getinv <-function(){
                     inver<-ginv(x)
                     inver %*% x     #The function calculates the inverse of a matrix
                     }
  list(set= set, get= get, 
       setinv = setinv,
       getinv = getinv)
}


## Function developed to obtain cached data (not repeat calculations over again)

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
                    message("getting chached")
                    return(inv)  ## provides the inverse values
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
f <-makeCacheMatrix(matrix(1:8, 2, 4))
f$get()
f$getinv()
cacheSolve(f)
