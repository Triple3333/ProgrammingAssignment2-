makeCacheMatri<-function(x=matrix()){
  m <- NULL               ## 定义局部变量m，初始值为NULL；
  set <- function(y) {  ## 定义一个函数set，参数y
    x <<- y           ## 调用set函数，会把参数x的值修改为y
    m <<- NULL       ## 同时把之前定义的m重置成NULL；
  }
  get <- function() {  ## 定义一个函数get，无参数 
    x                 ## 调用get函数，会返回x的值；
  }            
  setmatrix <- function(matrix) { ## 定义一个函数setmatrix，参数matrix
    m <<- matrix       ## 调用setmatrix函数，会把m的值修改为matrix
  }
  getmatrix <- function() {   ## 定义一个函数getmatrix，无参数 
    m                 ## getmatrix函数能返回m的值；
  }          
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  ## 返回一个list，这个list由4个函数组成
}
}


cacheSolve<-function(x){  ##建立函数cachesolve，参数为X，限定类型为矩阵
  m<-x$getsolve() ##在之前的结果集里寻找x的逆矩阵的结果
  if(!is.null(m)){ ##如果找到了
    message("I find it")##输出找到了
    return(m)##将结果返回
  }
  ## 如果代码执行到这里，说明之前的if语句被跳过，也就说明m为空
  data<-x$solve()##如果没找到，  读取缓存的vector/matrix
  m<-solve(x) ##计算x的逆矩阵
  x$setmatrix(m)##将计算结果存入x中
  m##返回结果
}
