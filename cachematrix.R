## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##读入数据函数set，当读入新的数据，m为空
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##得到原数据函数get
  get <- function() x
  ##记录逆矩阵函数setsolve
  setsolve <- function(solve) m <<- solve
  ##得到逆矩阵函数getsolve
  getsolve <- function() m
  ##一个列表，包括了四个函数
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ##如果m不是空的，就直接读m的数据并给出提示
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##m是空的，则进行逆矩阵运算
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ##输出m（逆矩阵）
  m
}

