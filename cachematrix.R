## This function creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL                   #Initialize the inverse variable to NULL
  set<-function(y){         #setting new matrix and nullify inverse
    x<<-y
    inv<<-NULL
  }
  get<-function() x                         # to return the matrix       
  setmatrix<-function(solve) inv<<- solve     #to set the inverse matrix 
  getmatrix<-function() inv                   #returning the  inverse
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
  
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix above. If the inverse was already (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     
  inv<-x$getmatrix()                        # Inverse will either be NULL or chached inverse matrix
  if(!is.null(inv)){
    message("getting the cached data when exists")
    return(inv)                           #returns chache inverse                         
  }
  matrix<-x$get()                       #stores matrix x in variable matrix
  inv<-solve(matrix, ...)                 #calculate the inverse matrix
  x$setmatrix(inv)                        #store the inverse matrix
  inv                                     #return chached inverse
  
}
