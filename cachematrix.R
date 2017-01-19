#### Create a list of 4 functions set, get, setinv, getinv
#### the make CacheMatrix will also store the original as well as inverse matrices
#### initialize Inverse matrix to NULL

### to test, after running source("makecachematrix.R")
### create a matrix (I used one of randon numbers, e.g. y<-matrix(rnorm(100),10,10))
### run x<-makeCacheMatrix(y)
### run cashSolve(x)
### the second time around you run it, it shows the 'getting cached data' message
### other checks: rerun makeCacheMatrix - it re-initializes the original matrix, so restes the inverse to null


# I did this assignemnt two years ago - I looked it over, it's fine, so I'll resubmit


makeCacheMatrix<-function(x=matrix())
{
    
    inverse<-NULL
    set<-function(y)
    {
        x<<-y
        inverse<<-NULL

    }
    get<-function() x
    setinv<-function(y) inverse<<-y
    getinv <-function() inverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve<-function(x)
{
    inv<-x$getinv()
   if(!is.null(inv) )
       {
       message('getting cached data')
        return(inv)
##### if the inverse already exists, don't recalculate
        }
##### at first pass, the inverse is not there so need to calculate using 'solve'
   data<-x$get()
   inv<-solve(data)
  x$setinv(inv)
   inv
}