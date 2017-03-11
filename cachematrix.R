# makeCacheMatrix (x)  creates special matrix object that can cache it´s inverse  
# inverse :=  matrix %*% inversematrix = unitymatrix
#
# Following the example makeVector an cachemean in the coursera course
# To stress that it is about matrices:  internal variable names beginn with an 'm'

# tested with square testmatrix smat <- matrix(c(2,1,5,3),2,2)
# which yields to   smatinv <- solve(smat)
# > smatinv
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2
#
#####
# tested with 
# ma <- makeCacheMatrix(smat)
# ma$get()  # displays the contents
# > ma$get()
# [,1] [,2]
# [1,]    2    5
# [2,]    1    3
# > ma$getinv()
# NULL
# > cacheSolve(ma)
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2
# > ma$getinv()
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2



makeCacheMatrix <- function(x = matrix()) {
  minv <-NULL  # set in makeCacheMatrix Environment
  set <-function (my) {
    x <<- my #  set mx in the global env
    minv <<-NULL
  }
  get <- function() x
  setinv <- function (inverse) minv <<- inverse
  getinv <-  function () minv
  list (set =set, get=get, setinv=setinv, getinv=getinv)
  
}

## cachesolve operates on the object created by makeCacheMatrix

  cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      minv <- x$getinv ()
      if (!is.null(minv)) {
        message ("get cached inverse matrix")
        return (minv)
      }
        
      matrix <- x$get()
      minv <-solve (matrix)
      x$setinv(minv)
      minv
  }

  
  # However - I think I did not really catch the point. 
  # When inverting matrices interactively, there´s a matrix object and an object containing the inverted matrix
  # both consume memory. Compute once and reuse the results as needed.
  #
  # When running in a sort of batch mode also the matrix and the inverse matrix are computed and stored.
  # Have the inverse computed once and it is available.   The clue is probably that with this cache method 
  # the storage and access is easier since I can access both the matrix and the inverse matrix by refering to 
  # special object created by makecachematrix instead of the matrix objects themselves 
  # 
  # I will think about this. Hope I solved the request nevertheless.
  
  
  
