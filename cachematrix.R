## The functions create the inverse matrix 
## from the MASS libary

## makeChaheMatrix consists of of set, get, setinv, getinv fuctions

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL # initializes inverse as NULL
  set <- function(y){
                    x<<-y
                    inv<<-NULL
                    }
  get<-function()x    #function to get matrix X
  setinv <- function(inverse)inv<<-inverse
  getinv <-function(){
                     inver<-ginv(x)
                     inver %*% x     #function to obtain the inverse of matrix
                     }
  list(set= set, get= get, 
       setinv = setinv,
       getinv = getinv)
}


## This function is used to obtain cached data (not repeat calculations over and over)

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
                    message("getting chached data!")
                    return(inv)  ## returns inverse values
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
