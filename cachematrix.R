
rm(list=ls())
#matrix for function to get  inverse 
MatCheckfun <- function(x = matrix()) {
  invevar1 <- NULL
  #set function to set the values 
  Setfunction <- function(y) {
    x <<- y
    invevar1 <<- NULL
  }
  #returns values of x the stored values 
  Getfunction <- function() x
  #Setes the value to the inverse varible 
  Setinversefunction <- function(inverse) invevar1 <<- inverse
  #it returns the value of inv
  Getinversefunction <- function() invevar1
  #Assignes the values 
  list(Setfunction=Setfunction, Getfunction=Getfunction, Setinversefunction=Setinversefunction, Getinversefunction=Getinversefunction)
}

#To calicualte the cache of the matrix if already there it should be returned or else it is newly caliculated 
cacheSolFuction <- function(x, ...) {
  invevar1 <- x$Getinversefunction()
  #Checking for the cached values  
  if(!is.null(invevar1)) {
    message("getting cached data.")
    return(invevar1)
  }
  ##Here we are assigning the values 
  data <- x$Getfunction()
  invevar1 <- solve(data)
  x$Setinversefunction(invevar1)
  invevar1
}
#Creating a 4*4 matrix for sending the values 
Mat1 = rbind(c(1, 7), c(8, 1))
#Calling the function of matrix  
UseChe = MatCheckfun(Mat1)
UseChe$Getinversefunction()
#Getting a function 
UseChe$Getfunction()
#Caling th cache funtion 
cacheSolFuction(UseChe)
cacheSolFuction(UseChe)

