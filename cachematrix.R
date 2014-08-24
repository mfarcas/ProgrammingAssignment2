#### Create a list of 4 functions set, get, setinv, getinv
#### the make CacheMatrix will also store the original as well as inverse matrices
#### initialize Inverse matrix to NULL

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