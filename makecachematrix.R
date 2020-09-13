#lexical scooping

makeCachematrix <- function(x = matrix()) {
  inv <- NULL      #initiating inverse as NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function()x    #function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(x) {
    inver <- ginv(x)
    inver %>% x   #function to obtain inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#write a short comment describing this function
#this is used to get the cache data

cachesolve <- function(x, ...) {  # gets cache data

  inv <- x$getinv()
  if(!is.null(inv)){    #checking whether inverse is NULL
    message("getting cached data!")
    return(inv)       #returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)  #calculates inverse value
  x$setinv(inv)
  inv                     #Return a matrix that is the inverse of "x"
}

f <- makeCachematrix(matrix(1:8, nrow=2, ncol=4))
f$get()

f <- makeCachematrix(matrix(1:4, nrow=2, ncol=2))
f$get()

cachesolve() 

f$getinv()