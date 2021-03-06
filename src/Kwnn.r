# Евклидово расстояние
euclideanDistance <- function(u, v) {
  sqrt(sum((u - v)^2))
}

# сортировка объектов Xl относительньно произвольного объекта u
# возвращает перестановку
orderByDist <- function(u, xl, metric = euclideanDistance) {
  distances <- c()
 #nrow(xl) - количество строчек выборки
  for (i in 1:nrow(xl)) {
    distances[i] <-metric(u, xl[i,])
  }
  
  return (order(distances))
}

# функция веса
weight <- function(i, k) {
  return((k+1-i)/k)
}

# kwNN
kwNN <- function(train, test, cl, k , weightFunc) {
  weights <- weightFunc(1:k, k)
  
  res <- c()
  for (i in 1:nrow(test)) {
    #Сортируем расстояния до точки
    order <- orderByDist(test[i,], train)
    
    ## Получаем классы первых k соседей
    classes <- cl[order[1:k]]
    
    names(weights) = classes
    ##Для каждого класса суммируем их вес
    classWeights <- sapply(names(table(classes)), function(class) {
        sum(weights[names(weights) == class])
    })
 
    ## Находим класс, который доминирует среди первых k соседей
    class <- names(which.max(classWeights))
    
    res[i] <- class
  }
  
  return (res)
}

# LOO
LOO <- function(xl) {
  n <- nrow(xl)
  maxk <- 20
  loo <- numeric(maxk)
  
 
  for (k in 1:maxk) {
    
    for (i in 1:n)   {
      pred <- kwNN(train = xl[-i, 3:4], test = xl[i, 3:4], cl = xl$Species, k = k, weight)
      loo[k] <- loo[k] + (pred != xl$Species[i]) 
    }
    
  }
  
  loo <- loo/n
}

drawLOO <- function(xl) {
  n <- nrow(xl)
  maxk <- 20
  loo <- LOO(xl)
  bestK <- which.min(loo)
  
  plot(1:maxk, loo, type = 'l', col = 'red', lwd = 2,
       xlab = 'k', ylab='LOO')
  
  points(bestK, loo[bestK], col = 'green3', bg = 'green3', asp = 1, pch = 21)
  
  legend( x="topright", 
<<<<<<< HEAD
          legend=c("Скользящий контроль","оптимальное k"), 
=======
          legend=c("Скользящий контроль","оптимальное k k"), 
>>>>>>> cd4964d83dde11a59d8db1a615114d102ed79419
          col=c("red","green3"), bg=c(NA, 'green3'), lwd=2, lty=c(1,NA), 
          pch=c(NA,19), merge=FALSE, cex=0.8 )
  
  text(bestK, loo[bestK], paste("k=", bestK), col = 'black', pos=3)
}

# картa	классификации
drawKwNN <- function(train, classes, colors) {
  plot(train, pch = 21, bg = colors[classes], col = colors[classes], asp = 1)
  
  step <- 0.1
  ox <- seq(0, 7, step)
  oy <-seq(0, 3, step)
<<<<<<< HEAD
  #всевозможные варианты перебора точек
=======
  # Перебор всех x  и y
>>>>>>> d69678164e3c2895479e25a7a7632b6853d9614b
  test <- expand.grid(Petal.Length = ox, Petal.Width = oy)
  
  prediction <- kwNN(train, test, classes, k = 4, weight)
  
  points(test, pch = 21, col = colors[prediction], asp = 1)
}

trainIris <- iris[, 3:4]
classes <- iris[, 5]
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

<<<<<<< HEAD
drawKwNN(trainIris, classes, colors)
#drawLOO(iris)
<<<<<<< HEAD
=======
#drawKwNN(trainIris, classes, colors)
drawLOO(iris)
>>>>>>> aa82e7e77d3ae374bb7246ab841d4b649e3ca4cd
=======
>>>>>>> cd4964d83dde11a59d8db1a615114d102ed79419
