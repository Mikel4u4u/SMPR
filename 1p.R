
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
oNN <- function(xl, z, metricFunction =euclideanDistance){
  
  
  min_dist = 1e14
  
  
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  for (i in 1:l){
    tmp_dist =metricFunction(xl[i, 1:n], z)
    
    if(tmp_dist < min_dist) {
      min_dist = tmp_dist
      class = xl[i, n+1]
    }
  }
  
  return (class)
}

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")


iris30 = iris[sample(c(1:150), 60, replace=FALSE), 3:5]

plot(iris30[, 1:2], pch = 21, bg = colors[iris30$Species],
     col = colors[iris30$Species])
xl <- iris30[, 1:3]

points_array = c()
x1<-0
x2<-0
while (x1<7) { 
  
  while (x2<3) { 
    z <- c(x1,x2)
    class <- oNN(xl, z)
    points_array = c(points_array, c(z))
    points(z[1], z[2], pch = 1, col = colors[class])
    x2<-x2+0.1
  }
  x2<-0
  x1<-x1+0.1
}

