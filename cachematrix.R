# The following functions go as a pair.They are constructed by taking advantage of the fact
# that R supports lexical scoping and that functions that return objects of type list() also allow access 
# to any other objects defined in the environment of the original function.The goal is to minimize the
# time it takes to compute the inverse of a matrix especially if it has to be computed repeatedly,
# an operation which can be time consuming if we have to deal with a big matrix object.
# Finally,in this assignment we assume that the matrix supplied is always invertible.

# The following link (it requires to  log in to coursera to open it) is a post from the discussion forum 
# which contains simple test matrices , in order to review my assignment.I post it here in case you want
# to validate the results.
#https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  set_inverse<-function(inverse) i<<-inverse
  get_inverse<-function() i
  list(set=set,get=get,
       set_inverse=set_inverse,get_inverse=get_inverse)
}

# Without cacheSolve, the makeCacheMatrix is an incomplete function. As designed,cacheSolve()
# is required to populate and/or retrieve the inverse of an object of type makeCacheMatrix.

cacheSolve<-function(x, ...){
  i<- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$set_inverse(i)
  i
  
}




