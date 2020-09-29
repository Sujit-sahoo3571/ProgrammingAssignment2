
# get matrix input (f$get())
# set value of matrix inverse by cachesolve() here (cachesolve(f))
# call inverse (f$getmean())

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}



cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # solve() for inverse matrix
        m <- solve(data, ...)
        x$setmean(m)
        m
        
        
}

# Test 1

#f <- makeCacheMatrix(matrix(1:4,2,2))

# f$get()

#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

# f$getmean()

#NULL

# cacheSolve(f)

#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# cacheSolve(f)

#getting cached data

#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# f$getmean()

#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
