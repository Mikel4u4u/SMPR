
#   Суть Loo состоит в том, что из выборки удаляется элемент, алгоритм обучается на полученой выборке
#затем обученый алгоритм проверяется на исключенном элементе 
#Поступив таким образом со всеми элементами выборки, можно судить о методе обучения по
#частоте ошибочных классификаций.
xl <- iris
l <- dim(xl)[1] 
n <- dim(xl)[2] - 1
N <- c(2,3)

euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
    
  }
  
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}



kNN <- function(orderedXl ,  k)
{
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  
  class <- names(which.max(counts))
  return (class)
}

k <- array(0,dim = l)



for (j in 1:l) {
  # Сортируем массив расстояний до j точки 
  mass1 <- xl[-j, ] 
  
  orderedXl <- sortObjectsByDist(mass1 , xl[j,1:n] )  
 
  l <- dim(xl)[1]
  
  n <- dim(xl)[2] - 1
  
  
  for (i in 1:l) {
  
    k[i] <- k[i] + (xl[ j , n+1 ] !=  kNN( orderedXl  , i) )
  
  }
  
}

for (j in 1:l) {
  k[j]<-k[j]/l

}
plot(1:l, k, type = 'h', col = "black", xlab = "k", ylab = "Q" )
