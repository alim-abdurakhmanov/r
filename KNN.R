mc.euclideanDist <- function(u,v){
  sqrt(sum((u-v)^2))
}

mc.sortObjectsByDist <- function(xl, u, metricFunction=mc.euclideanDist){
  
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  distances <- matrix(NA, l, 2)
  
  for(i in 1:l) {
    distances[i,] <- c(i, metricFunction(xl[i, 1:n], u))
  }
  
  orderedXl <- xl[order(distances[,2]),]
  return(orderedXl)
}

KNN <- function(xl,u,k){
  orderedXl <- mc.sortObjectsByDist(xl,u)
  n <- dim(orderedXl)[2]-1
  classes <- orderedXl[1:k, n+1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return(class)
}

mc.getLooFromKDependencies <- function(xl, metricFunction = mc.euclideanDist) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  loo <- array(0, l - 1)
  
  for(i in 1:l){
    u <- xl[i, 1:n]
    orderedXl <- mc.sortObjectsByDist(xl[-i,], u, metricFunction)
    
    for(k in 1:l-1){
      class <- KNN(orderedXl, u, k)
      
      if(class != xl[i, n+1]){
        loo[k] <- loo[k] + 1
      }
    }
    print(i)
  }
  loo <- loo / l
  return(loo)
}

nnDemo <- function(objectCount=50)
{
  class(iris)
  
  l <- dim(iris)[1]
  
  rows <- sample(1: l, objectCount, replace=FALSE)
  iris <- iris[rows, c("Sepal.Width", "Petal.Length", "Species")]
  
  l <- dim(iris)[1]
  n <- dim(iris)[2]-1
  
  par(mfrow=c(1, 2)) #2 графика в 1 окне
  
  colors <- c(setosa="red", versicolor="green3", virginica="blue")
  
  looo <- mc.getLooFromKDependencies(iris)
  k <- which.min(looo)
  
  plot(looo, xlab = "k", ylab = "LOO", type="l")
  points(k, looo[k], pch = 20, col = "blue")
  text(k, looo[k], col = "blue", labels = paste("k = ",toString(k)), pos=3 )
  
  lines(c(k,k),c(0,looo[k]), lty=3, col="blue")
  
  plot(iris[,1], iris[,2], pch=16, xlab="Ширина чашелистика", ylab="Длина лепестка", col=colors[iris[,n+1]], asp=1)
  
  x1 <- 0
  while(x1 < 6){
    x1 <-  x1 + 0.2
    
    x2 <- 0
    while(x2 < 7){ 
      
      x2 <-  x2 + 0.2 
      u <- c(x1, x2)
      
      classU <- KNN(iris, u, k)
      
      points(x1, x2, pch=3, col=colors[classU], asp=1)
    }
  }
  
}
