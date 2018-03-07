## These functions compute the inverse of a matrix
## and cache the inverse for later retrieval

## makeCacheMatrix creates a "matrix" 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m<-matrix()
      set <- function(y=matrix()) {
            x <<- y           #Assign input argument to parent env.
            m <<- matrix()    #Clear previous inverse matrix
      }      
      get <- function() x     #Retrieve the input matrix 
      
      setinv <- function(x) {  #Assign input value to m in the parent env.
            m<<-x
      }
      getinv <- function() m    #Retrieve the inverse matrix
      
      list(set = set, get = get,   #Assign names to all the functions
           setinv = setinv,        #for $ style referencing
           getinv = getinv)
}


## cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix if needed.  If the inverse has already been
## computed and the matrix has not changed since the inverse was
## computed, the function
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()     #Call function to retrieve inverse matrix
      if(is.numeric(m))  {    #If matrix is numeric, then it is valid inverse
            message("getting cached data")
            return(m)         #Print the inverse matrix
      }
      data <- x$get()  #If matrix is not valid, get input matrix
      m <- solve(data)   #Calculate inverse
      x$setinv(m)       #Call function to assign inverse matrix to parent env.
      m                 #Print the inverse matrix
}
      
      
      

