#Below there are created two functions. 
#makeCacheMatrix is used to create special list of “functions” to provide data to cacheSolve function and to chache inverse  matrix data (which data we get data for cacheSolve).
#cacheSolve function does actual calculation of inverse data using solve() function. Before calculation, cacheSolve checks for cached data of inverse matrix. If such data is found, function returns what is cached. If no inverse matrix data is cached for given matrix, it performs calculation using solve() function.  
#NOTE: functions are not made  failproof (i.e. no check if matrix is solvable or if passed object is matrix) as it was not required on assignment. 



#################
###MAKE MATRIX###
#################
#makeCacheMatrix is used to create special list of “functions” to provide data to cacheSolve function and to chache inverse  matrix data (which data we get data for cacheSolve).


makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL                                          #Set null value for Inverse matrix. So each time we run a fuction it allocates memmory for data and cleans cache
  
  set <- function(y) {                                 #we can  use set to change matrix data wihout calling whole makeCacheMatrix later
    x <<- y
    inv <<- NULL
    }
    
  get <- function() x                                  #get matrix data            
  setinv <- function(inverse) inv <<- inverse          #set inverse matrix data FROM cacheSolve 
  getinv <- function() inv                             #get inverse matrix data FOR cacheSolve 
  list(set = set, get = get,                           
       setinv = setinv,
       getinv = getinv)
}


#################
#####SOLVE#######
#################
#cacheSolve function does actual calculation of inverse data using solve() function. Before calculation, cacheSolve checks for cached data of inverse matrix. If such data is found, function returns what is cached. If no inverse matrix data is cached for given matrix, it performs calculation using solve() function.  

cacheSolve <- function(x, ...) {                        
  inv <- x$getinv()                                     #gets inverse matrix cached data from makeCacheMatrix object
  if(!is.null(inv)) {                                   # check if  inverse matrix cached data exists (is not null)
    message("getting cached data")                      # if  chache exists, it is returned by function with additional message for clarity
    return(inv)
  }
  data <- x$get()                                       #this is case where no chache for this matrix exist. Then matrix is provided for makeCacheMatrix
  inv <- solve(data, ...)                               #matrix is solved             
  x$setinv(inv)                                         #result is put in  makeCacheMatrix object's cache, so if run function again,  we will get cached data
  inv                                                   #returns inverse of matrix
}

