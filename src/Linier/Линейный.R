source('utils.R')

## Нормализация обучающей выборки
trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}

## Добавление колонки для из -1 для w0
trainingSamplePrepare <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  xl <- cbind(xl[, 1:n], seq(from = -1, to = -1,
                             length.out = l), xl[, n + 1])
}







# Кол-во объектов в каждом классе
ObjectsCountOfEachClass <- 100
## Моделируем обучающие данные
library(MASS)
Sigma1 <- matrix(c(2, 0, 0, 5), 2, 2)
Sigma2 <- matrix(c(4, 1, 1, 2), 2, 2)

xy1 <- mvrnorm(n=ObjectsCountOfEachClass, c(0, 0), Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, c(10, -5),Sigma2)
xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))
colors <- c(rgb(255/255, 255/255, 0/255), "white",
            rgb(0/255, 200/255, 0/255))
## Нормализация данных
xlNorm <- trainingSampleNormalization(xl)
xlNorm <- trainingSamplePrepare(xlNorm)
## Отображение данных
## ADALINE
plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3]
                                                     + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Линейные
     классификаторы")

w <- sg.ADALINE(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "blue")
## Правило Хебба
w <- sg.Hebb(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "green3")
## Логистическая регрессия
w <- sg.LogRegression(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "red")
legend("bottomleft", c("ADALINE", "Правило Хэбба",
                       "Логистическая регрессия"), pch = c(15,15,15), col =
         c("blue", "green3", "red"))