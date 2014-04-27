makeCacheMatrix <- function(x = numeric()) { 
        m <- NULL 
        
        # First Function "set()" 
        # "<<-" is the same as "<-" except it 
        # looks a level up to the parent environment, 
        # in this case makeCacheMatrix(). Since "m" is 
        # defined within makeCacheMatrix() NOT in set() 
        # so it must look outside of itself
        set <- function(y) { 
                x <<- y 
                m <<- NULL 
        } 
        
        # Second Function "get()" 
        get <- function() { 
                x 
        } 
        
        # Third Function "setinverse()" 
        
        setinverse <- function(solve) { 
                m <<- solve 
        } 
        
        # Fourth Function 
        getinverse <- function() { 
                m 
        } 
        
        # Return 
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse) 
} 
cacheSolve <- function(x, ...) { 
        m <- x$getinverse()           #query the x vector's cache         
        if(!is.null(m)) {           #if there is a cache 
                message("getting cached data") 
                return(m)                #just return the cache, no computation needed 
        } 
        data <- x$get()             #if there's no cache 
        m <- solve(data, ...)        #we actually compute them here 
        x$setinverse(m)                #save the result back to x's cache 
        m                           #return the result
}

