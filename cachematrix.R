#Assignment2
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 處理,儲存data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ##將y值賦給x
                x <<- y
                m <<- NULL
        }
        ##返回x
        get <- function() x
        ##將緩存結果賦給m
        setCacheMatri <- function(CacheMatri) m <<- CacheMatri
        ##返回m
        getCacheMatri <- function() m
        ##返回list
        list(set = set, get = get,
             setCacheMatri = setCacheMatri,
             getCacheMatri = getCacheMatri)
}


## Write a short comment describing this function
## 若計算過則調用資料

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #先查看是否計算過
        m <- x$getCacheMatri()
        ##若計算過則返回
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #沒計算過,調用x
        matrix <- x$get()
        #計算返矩陣
        m <- solve(matrix, ...)
        #將值緩存
        x$setCacheMatri(m)
        m        
}
