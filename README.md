# SMPR
<p><a href="#Vvonyye_opredeleniya">1. Вводные определения</a></p>
  <p><a href="#Metricheskiye_algoritmy">2. Метрические алгоритмы</a></p>
  <p><a href="#a1NN">2.1. 1NN</a></p>
  <p><a href="#akNN">2.2. kNN</a></p>
  <p><a href="#aLOO">2.3. LOO</a></p>
  <p><a href="#akwNN">2.4. kwNN</a></p>
  <p><a href="#aPW">2.5. Парзеновские окна</a></p>
  <p><a href="#aPF">2.6. Потенциальные функции</a></p>
  
  <p><a href="#aBaes">3. Байесовские классификаторы</a></p>
  <p><a href="#aNormal">3.1. Нормальный дискриминантный анализ</a></p>
  <p><a href="#aNNBaes">3.1.1 Наивный байесовские алгоритм</a></p>
  <p><a href="#aPlug">3.2. Подстановочный алгоритм</a></p>
  <p><a href="#aLinear">4. Линейные классификаторы</a></p>
  ## 1. Вводные определения <a name="Vvonyye_opredeleniya"></a>
  
  <p>Задано множество объектов X, и множество допустимых ответов Y, и существует целевая функция y*: X -> Y, значения которой y<sub>i</sub> = y*(x<sub>i</sub>) известны только на конечном подмножестве объектов {x<sub>1</sub>, …, x<sub>ℓ</sub>} ⊂ X.</p>
  <p>Пары «объект-ответ» (x<sub>i</sub>, y<sub>i</sub>) называются <i>прецедентами</i>. Совокупность пар 𝑋<sup>ℓ</sup>=(𝑥<sub>𝑖</sub>,𝑦<sub>𝑖</sub>)<sup>ℓ</sup><sub>𝑖=1</sub> называется <i>обучающей выборкой</i>.</p>
  <p>Задача <i>обучения по прецедентам</i> заключается в том, чтобы по выборке 𝑋<sup>ℓ</sup> <i>восстановить зависимость</i> y*, то есть построить <i>решающую функцию</i> a: X -> Y, которая приближала бы целевую функцию y*(x), причем не только на объектах обучающей выборки, но и на всем множестве X.</p>
  <p>Решающая функция a должна допускать эффективную компьютерную реализацию; по этой причине её называют <i>классифицирующим алгоритмом</i> .</p>
  <p><i>Признак f</i> объекта x – это результат измерения некоторой характеристики объекта. Формально признаком называется отображение <i>f</i>: X -> D<i>f</i>, где D<i>f</i> – множество допустимых значений признака. В частности, любой алгоритм a: X -> Y также можно рассматривать как признак.</p>
  <p>Набор признаков <i>f</i><sub>1</sub>,…, <i>f</i><sub>n</sub>. Вектор (<i>f</i><sub>1</sub>(x),…, <i>f</i><sub>n</sub>(x)) называют <i>признаковым описанием</i> объекта x ∈ X. В дальнейшем будем полагать, что X = 𝐷<sub><i>𝑓</i><sub>1</sub></sub> × … × 𝐷<sub><i>𝑓</i><sub>𝑛</sub></sub>.</p>
  <p>Совокупность признаковых описаний всех объектов выборки записанная в виде таблицы размера ℓ × n, называют <i>матрицей объектов-признаков</i>:</p>
  
  <p><i>Моделью алгоритмов</i> называется параметрическое семейство отображений A = {g(x, <i>θ</i>) | <i>θ</i> ∈ <i>Θ</i>}, где g: X × <i>Θ</i> -> Y – некоторая фиксированная функция, <i>Θ</i> – множество допустимых значений параметра <i>θ</i>, называемое <i>пространством параметров</i> или <i>пространством поиска</i>.</p>
  <p>Процесс подбора оптимального параметра модели <i>θ</i> по обучающей выборке 𝑋<sup>ℓ</sup> называют <i>настройкой</i> или <i>обучением</i> алгоритма a ∈ A.</p>
  <p><i>Метод обучения</i> – это отображение μ: (X × Y)<sup>ℓ</sup> -> A, которое произвольной конечной выборке 𝑋<sup>ℓ</sup>=(𝑥<sub>𝑖</sub>,𝑦<sub>𝑖</sub>)<sup>ℓ</sup><sub>𝑖=1</sub> ставит в соответствие некоторый алгоритм a ∈ A. Говорят также, что метод <i>строит</i> алгоритм a по выборке X<sup>ℓ</sup>.</p>
  <p><i>Функция потерь</i> – это неотрицательная функция ℒ(a, x), характеризующая величину ошибки алгоритма a на объекте x. Если ℒ(a, x) = 0, то ответ a(x) называется <i>корректным</i>.</p>
  <p>Другие названия функционала качества – <i>функционал средних потерь</i> и <i>эмпирический риск</i>.</p>
  <p>Если функция потерь принимает значения 1 – ошибочная классификация или 0 – корректная классификация, то функция потерь называется <i>бинарной</i> или <i>индикатором ошибки</i>, а функционал качества называется <i>частотой ошибок</i> алгоритма на выборке.</p>
  <p>Часто используют:</p>
  <p>ℒ(a, x) = |a(x) – y*(x)| - отклонение от правильного ответа; функционал качества тогда зовут – <i>средней ошибкой</i>.</p>
  <p>ℒ(a, x) =(a(x) – y*(x))2 – квадратичная функция потерь; функционал качества – <i>среднеквадратичной ошибкой</i>.</p>

  
  ## 2. Метрические алгоритмы  <a name="Metricheskiye_algoritmy"></a>
  
  ### 2.1. 1NN  <a name="a1NN"></a>
  
  <p><ol>
    <li>Подбирается метрика. В данном методе используется евклидова метрика .</li>
    <p><img src="img\евклидовам.png"></p>
    <li>Обучающая выборка сортируется в порядке увеличения расстояния от классифицируемого элемента.</li>
    <li>Элемент относят к тому классу к которому принадлежит ближайший (первый в отсортированной выборке) элемент.</li>
    <img src="img\sort.png">
  
  </ol></p>
  
  ### 2.2. kNN  <a name="akNN"></a>
  
  <p>Алгоритм 1NN чувствителен к <i>выбросам</i>-случаям, когда 1 или несколько элементов одного класса оказываются среди элементов другого, устранить эти ситуации может алгоритм kNN.</p>
  <p>Алгоритм kNN отличается от 1NN тем что он относит классифицируемый элемент не к классу ближайшего к нему элемента, а к классу чаще всего встречающемуся среди k ближайших элементов.</p>
  <img src="img\kNNs.png">
  
  <p>Иллюстрация резултатов работы алгоритмов 1NN и kNN(k=4):</p>
    1NN
  <p><img src="img\1NN.png" ></p>
    kNN , k = 4
  <p><img src="img\kNN.png"></p>
  
  ### 2.3. LOO  <a name="aLOO"></a>
  Оптимальное в смысле точности предсказаний значение  `k`  может быть найдено с использованием перекрестной проверки. Для этого по фиксированному значению  `k`  строится модель  `k` -ближайших соседей и оценивается ошибка классификации. Эти действия повторяются для различных  `k`  и значение, соответствующее наименьшей ошибке распознавания, принимается как оптимальное.

 <p><img src="img\Loo1.png" ></p>
 
  ### 2.4. kwNN <a name="akwNN"></a>

i-му соседу объекта u приписывается вес ![](https://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%2Cu%29), убывающий с ростом ранга соседа i. Объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.

Выберем следующую функцию веса ![](https://latex.codecogs.com/svg.latex?%5Clarge%20w_i%20%3D%5Cfrac%7B%28k%20&plus;%201-i%29%7D%7Bk%7D)
 
#### Критерий	скользящего	контроля	LOO для kwNN

![LOO kNN](img/LOOkwnn.png)

#### Карта	классификации	kwNN , k = 4

![kNN](img/kwNN.png)
---

#### Сравнение	качества	алгоритмов	kNN и	kwNN.

kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации. 

kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улчшая качество классификации.

#### Пример,	показывающий	преимущество	метода kwNN над kNN.
	
Недостаток kNN в том, что максимальная сумма голосов может достигаться на нескольких классах одновременно.
В задачах с двумя классами этого можно избежать, если брать только нечётные значения k. Более общая тактика, которая годится и для случая многих классов — ввести строго убывающую последовательность вещественных весов, задающих вклад i-го соседа в классификацию.



### 2.5. PW(Парзеновские окна) <a name="aPw"></a>
Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20K%28%5Cfrac%7B%5Crho%28u%2C%20x%5Ei_u%29%7D%7Bh%7D%29)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

Парзеновская оценка плотности имеет вид:

![](https://latex.codecogs.com/gif.latex?a(x;X^{l},h)=arg\max_{y\epsilon&space;Y}\lambda&space;_{y}\sum_{i:y_{i}=y}&space;K(\frac{\rho(x,x_{i})}{h}))


Алгоритм для классифицируемой точки _u_ строит
окружность, радиусом _h_. Все точки, не попавшие в эту окружность,
отсеиваются (кроме гауссовского). Для остальных, вычисляется вес,
суммируется, и класс с наибольшим весом считается победителем.

Программная реализация алгоритма:
```
PW = function(distances, u, h) {
    weights = PW.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, function(class, arr) { sum(arr[names(arr) == class]) } , weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}
```
где `distances` – расстояние от точки `u` до всех точек выборки,
`names(distances)` – наименование классов точек выборки.

Параметр ширины `h` раасчитывается с помощью `Loo`    :
```
LOOPW = function(points, classes, hValues) {
  n = dim(points)[1]
  loo = rep(0, length(hValues))
  
  for (i in 1:n) {
    u = points[i,]
    sample = points[-i,]
    distances = distances(sample, u)
    names(distances) = classes[-i]
    
    for (j in 1:length(hValues)) {
      h = hValues[j]
      classified = PW(distances, u, h)
      loo[j] = loo[j] + (classified != classes[i])
    }
  }
  
  loo = loo / n
}
```
`h = hValues[which.min(loo)]` 






Чаще всего применяются 5 типов ядер:
- Прямоугольное ![](  http://latex.codecogs.com/svg.latex?%5Clarge%20R%28z%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5B%7Cz%7C%20%5Cleq%201%5D )
![kNN](img/RPW.png)
- Треугольное ![]( http://latex.codecogs.com/svg.latex?%5Clarge%20T%28z%29%20%3D%20%281%20-%20%7Cz%7C%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D )
![kNN](img/TPW.png)
- Квартическое ![]( http://latex.codecogs.com/svg.latex?%5Clarge%20Q%28z%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D%20%281%20-%20z%5E2%29%5E2%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D )
![kNN](img/QPW.png)
- Епанечниково ![]( http://latex.codecogs.com/svg.latex?%5Clarge%20E%28z%29%20%3D%20%5Cfrac%7B3%7D%7B4%7D%20%281%20-%20z%5E2%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D )
![kNN](img/EPW.png)
- Гауссовское (нормальное распределение)
![kNN](img/GPW.png)

### 2.5. Потенциальные функции <a name="aPF"></a>

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?W_y%28i%2C%20u%29%20%3D%20%5Cgamma_i%20%5Ccdot%20K%28%5Cfrac%7B%5Crho%28u%2C%20x_u%5Ei%29%7D%7Bh_i%7D%29%2C%20%5Cgamma_i%20%5Cgeq%200%2C%20h_i%20%3E%200)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

Алгоритм для каждого обучающего объекта _x_ строит
окружность, радиуса _h_ и силы воздействия (потенциала)
![](http://latex.codecogs.com/svg.latex?%5Cgamma_i).

Программная реализация функции классификации:
```
PF = function(distances, potentials, h) {
    weights = potentials * PF.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, sumByClass, weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}
```
Однако, прежде, чем использовать потенциалы, их необходимо подобрать. 
Изачально потенциалы заполняются нулями. Далее, пока количество ошибок классификации
не достигнет нужного предела, выбираем случайно точку _x_ из выборки. Если для нее
классификация выполняется неверно, увеличиваем потенциал на 1 и пересчитываем
общее количество ошибок.

Ядро квартическое , _h_ для красных 1 , для остальных 0.25
![Потенциалы PF...](img/PF.png)


Ядро  <a href="img/Rplot.png">гаусовское</a>   

## 3.Байесовские классификаторы <a name="aBaes"></a>

Байесовский подход к классификации основан на теореме, утверждающей,
что если плотности распределения каждого из классов известны,
то искомый алгоритм можно выписать в явном виде.
Более того, этот алгоритм оптимален,
то есть обладает минимальной вероятностью ошибок.

Для классифицируемого объекта вычисляются функции правдоподобия каждого
из классов, по ним вычисляются апостериорные вероятности классов.
Объект относится к тому классу, для которого __апостериорная вероятность
максимальна__.

![](https://latex.codecogs.com/svg.latex?a%28x%29%20%3D%20%5Carg%20%5Cmax_%7By%20%5Cin%20Y%7D%20%5Clambda_y%20P_y%20p_y%28x%29)

На практике _плотности распределения классов_, как правило, не известны.
Их приходится оценивать (восстанавливать) по обучающей выборке.
В результате байесовский алгоритм перестаёт быть оптимальным,
так как восстановить плотность по выборке можно только с некоторой погрешностью.

__Разделяющая поверхность__ между классами _t_ и _s_ – это геометрическое
место точек
![](https://latex.codecogs.com/svg.latex?x%20%5Cin%20X)
таких, что _максимум апостериорной вероятности_ достигается одновременно
при _y = s_ и _y = t_.

![](https://latex.codecogs.com/svg.latex?%5Clambda_t%20P_t%20p_t%28x%29%20%3D%20%5Clambda_s%20P_s%20p_s%28x%29).

Следующие алгоритмы по исходным выборкам восстанавливают _плотности распределения
классов_ и отдеяют классы друг от друга при помощи
_разделяющей поверхности_.

### Нормальный дискриминантный анализ <a name="aNormal"></a>

Это специальный случай баесовской классификации, когда предполагается, что плотности
всех классов
![](http://latex.codecogs.com/svg.latex?p_y%28x%29%2C%20y%20%5Cin%20Y)
являются многомерными нормальными. В этом случае задача решается аналитически.
Сами плотности вычисляются по формуле:

![](http://latex.codecogs.com/svg.latex?N%28x%3B%5Cmu%2C%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5En%20%7C%5CSigma%7C%7D%7D%20%5Ccdot%20exp%5Cleft%28-%5Cfrac%7B1%7D%7B2%7D%28x%20-%20%5Cmu%29%5ET%20%5CSigma%5E%7B-1%7D%20%28x%20-%20%5Cmu%29%5Cright%29),
в которой

![](http://latex.codecogs.com/svg.latex?x%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– объект, состоящий из *n* признаков,

![](http://latex.codecogs.com/svg.latex?%5Cmu%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– математическое ожидание,

![](http://latex.codecogs.com/svg.latex?%5CSigma%20%5Cin%20%5Cmathbb%7BR%7D%5E%7Bn%20%5Ctimes%20n%7D)
– ковариационная матрица (положительно определенная, симметричная, невырожденная).

#### Геометрия нормальной плотности

1. Если признаки некореллированы, то есть
![](http://latex.codecogs.com/svg.latex?%5CSigma%20%3D%20%5Cmbox%7Bdiag%7D%28%5Csigma_1%5E2%2C...%2C%5Csigma_n%5E2%29)
, то плотности распределения имеют форму эллипсоидов, параллельных осям координат:

![](img/line1.png)

Матрица ковариации : 
```
c(3,0,

 0,1)
 ```
![](img/line11.png)

Матрица ковариации : 
```
c(1,0,

  0,3)
```

2. Если признаки имеют одинаковые дисперсии
![](http://latex.codecogs.com/svg.latex?%5CSigma%20%3D%20%5Csigma%5E2I_n),
линии уровня имеют форму эллипсоидов:

![](img/line2.png)

Матрица ковариации :
```
c(1,0,

  0,1)
```
3. Если матрица не диагональна, то линии уровня – эллипсоиды, повернутые относительно
оси координат:

![](img/line3.png)

Матрица ковариации : 
```
c(1,1,

  0,1)
```
### Наивный баесовский алгоритм <a name="aNNBaes"></a>

<p>Если признаки элементов независимые случайные величины, то функции правдоподобия классов примут вид</p>
  <p>p<sub>y</sub>(x) = p<sub>y1</sub>(ξ<sub>1</sub>)⋅⋅⋅p<sub>yn</sub>(ξ<sub>n</sub>)</p>
  <p>где p<sub>yj</sub>(ξ<sub>j</sub>) плотность распределения значений j-го признака для класса y.</p>
  <p>Подставив в байесовское решающее правило эмпирические оценки одномерных плотностей признаков получим алгоритм</p>
  <p><img src="img/naivnyy_bayes.jpg" ></p>
  

  <p><img src="img/NNBaes.png" alt="Визуализация plug-in"></p>

### Подстановочный алгоритм (Plug-in) <a name="aPlug"></a>

__Подстановочный алгоритм__ относится к нормальному дискриминантному анализу.

Чтобы узнать _плотности распределения классов_, алогритм восстанавливает
неизвестные параметры
![](http://latex.codecogs.com/svg.latex?%5Cmu%2C%20%5CSigma)
по следующим формулам для каждого класса _y_ :

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5Cmu%7D%20%3D%20%5Cfrac%7B1%7D%7Bl_y%7D%20%24%24%5Csum_%7Bi%20%3D%201%7D%5E%7Bl_y%7D%20x_i%24%24)

```
estimateMu = function(points) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    mu = matrix(NA, 1, cols)
    for (col in 1:cols) {
        mu[1, col] = mean(points[, col])
    }
    return(mu)
}
```

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5CSigma%7D%20%3D%20%5Cfrac%7B1%7D%7Bl_y%20-%201%7D%20%24%24%5Csum_%7Bi%20%3D%201%7D%5E%7Bl_y%7D%20%28x_i%20-%20%5Chat%7B%5Cmu%7D%29%28x_i%20-%20%5Chat%7B%5Cmu%7D%29%5ET).

```
estimateCovarianceMatrix = function(points, mu) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    covar = matrix(0, cols, cols)
    for (i in 1:rows) {
        covar = covar + (t(points[i,] - mu) %*% (points[i,] - mu)) / (rows - 1)
    }
    return(covar)
}
```

Программа доступна по ссылке:
[shinyapps.io](https://mikel4u4u.shinyapps.io/Desktop/).

Алгоритм довольно точно восстанавливает _ковариационную матрицу_ и
_мат. ожидание_ объектов выборки.

_Разделяющая кривая  парабола_
![](img/plug-in-3.png)

Однако при малом количестве количестве объектов, точность падает:

_Разделяющая кривая  элипс_
![](img/plug-in-2.png)


_Разделяющая кривая  гипербола_
![](img/plugin3.png)

## 4.Линейные классификаторы <a name="aLinear"></a>
Пусть ![](http://latex.codecogs.com/svg.latex?X%20%3D%20%5Cmathbb%7BR%7D%5En)
и ![](http://latex.codecogs.com/svg.latex?Y%20%3D%20%5C%7B-1%3B&plus;1%5C%7D).
Алгоритм

![](http://latex.codecogs.com/svg.latex?a%28x%2Cw%29%3D%20%5Ctext%7Bsign%7Df%28x%2Cw%29%3D%5Ctext%7Bsign%7D%20%28%5Clangle%20w%2Cx%20%5Crangle-w_0%29%2Cw%20%5Cin%20%5Cmathbb%7BR%7D%5En)

является __линейным алгоритмом классификации__.
Если _f_>0, то алгоритм _a_ относит _x_ к классу +1. Иначе к классу -1.

Отсюда
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0)
называется __уравнением разделяющей поверхности__.

Параметр ![](http://latex.codecogs.com/svg.latex?w_0) иногда опускают. Однако в таком
случае разделяющая поверхность (в нашем случае с 2мя признаками – прямая),
соответствующая уравнению
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0),
будет всегда проходить через начало координат. Чтобы избежать такого обобщения,
будем полагать, что среди признаков _x_ есть константа
![](http://latex.codecogs.com/svg.latex?f_j%28x%29%20%5Cequiv%20-1),
тогда роль свобоного коэффициента ![](http://latex.codecogs.com/svg.latex?w_0)
играет параметр ![](http://latex.codecogs.com/svg.latex?w_j).
Тогда разделяющая поверхность имеет вид
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle=w_j).

Величина
![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3Dy_i%5Clangle%20x_i%2Cw%20%5Crangle)
называется __отступом__ объекта относительно алгоритма классификации. Если
![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3C0),
алгоритм совершает на объекте
![](http://latex.codecogs.com/svg.latex?x_i)
ошибку.

![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29)
– монотонно невозрастающая __функция потерь__, мажорирует пороговую функцию
![](http://latex.codecogs.com/svg.latex?%5BM%3C0%5D%20%5Cleq%20%5Cmathcal%7BL%7D%28M%29).
Тогда __минимизацю суммарных потерь__ можно рассматривать как функцию вида
![](http://latex.codecogs.com/svg.latex?%5Ctilde%7BQ%7D%28w%2CX%5E%5Cell%29%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%28M_i%28w%29%29%5Crightarrow%20%5Cmin_w)

#### Метод стохастического градиента

Для минимизации
![](http://latex.codecogs.com/svg.latex?Q%28w%29)
применяется __метод градиентного спуска__.

В начале выбирается некоторое _начальное приближение вектора весов_ _w_.
Не существует единого способа инициализации весов. Хорошей практикой считается
инициализировать веса случайными малыми значениями:
![](http://latex.codecogs.com/svg.latex?w_j%3A%3D%5Ctext%7Brandom%7D%28-%5Cfrac%7B1%7D%7B2n%7D%2C&plus;%5Cfrac%7B1%7D%7B2n%7D%29)
, где _n_ – количество признаков _x_.

Далее высчитывается _текущая оценка функционала_
![](http://latex.codecogs.com/svg.latex?Q%3A%3D%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)

Затем запускается итерационный процесс, на каждом шаге которого вектор _w_
изменяется в сторону наиболее быстрого убывания _Q_. Это направление противоположно
вектору градиента
![](http://latex.codecogs.com/svg.latex?Q%27%28w%29). Соответственно веса меняются по
правилу:

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%20Q%27%28w%29)

или

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%27%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29x_iy_i),

где
![](http://latex.codecogs.com/svg.latex?%5Ceta%3E0)
– __темп обучения__. Чтобы не проскочить локальный минимум темп обучания принято
полагать небольшим. Однако, при слишком маленьком его значении алгоритм будет
медленно сходится. Хорошей практикой считается его постепенное уменьшение по ходу
итераций. Мы же будем полагать его равным
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Ctext%7Biteration%7D%7D).

__Критерий останова__ основан на приблизительной оценке _Q_ методом
_экспоненциальной скользящей средней_:

![](http://latex.codecogs.com/svg.latex?Q%3D%281-%5Clambda%29Q&plus;%5Clambda%20%5Cvarepsilon_i)
,

где
![](http://latex.codecogs.com/svg.latex?%5Cvarepsilon_i%3D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)
– __ошибка__ алгоритма на случайном элементе
![](http://latex.codecogs.com/svg.latex?x_i),

![](http://latex.codecogs.com/svg.latex?%5Clambda) – __параметр сглаживания__,
полагаем его равным
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Cell%7D).

Алгоритм может остановиться в двух случаях:

1. Он не допускает ошибки ни на каком элементе;
2. Значение _Q_ стабилизировано, то есть
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B%7CQ_%7Bprev%7D-Q%7C%7D%7B%5Cmax%28Q_%7Bprev%7D%2CQ%29%7D%20%5Cleq%201e-5).
``` 
if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
```

<u>Примечание.</u> Градиентный метод чувствителен к масштабу измерения признаков.
Если норма _x_ большая, итерационный процесс может оказаться парализованным.
Чтобы этого не произошло, рекомендуется _нормализовать_ признаки:

![](http://latex.codecogs.com/svg.latex?x%5Ej%3A%3D%5Cfrac%7Bx%5Ej-x%5Ej_%5Ctext%7Bmin%7D%7D%7Bx%5Ej_%5Ctext%7Bmax%7D-x%5Ej_%5Ctext%7Bmin%7D%7D)
, где
![](http://latex.codecogs.com/svg.latex?x%5Ej)
– _j_-й признак.

В обобщенном виде алгоритм _стохастического градиента_ выглядит следующим образом:

```
stoh = function(xl, classes, L, updateRule) {
    #изначальная настройка алгоритма
    rows = dim(xl)[1]
    cols = dim(xl)[2]
    w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
    lambda = 1 / rows

    # начальное Q
    Q = 0
    for (i in 1:rows) {
        margin = sum(w * xl[i,]) * classes[i]
        Q = Q + L(margin)
    }
    Q.prev = Q

    iter = 0
    repeat {
        iter = iter + 1

        # выбрать объекты с ошибкой
        margins = rep(0, rows)
        for (i in 1:rows) {
            xi = xl[i,]
            yi = classes[i]
            margins[i] = sum(w * xi) * yi
        }
        errorIndecies = which(margins <= 0)

        #выходим, если выборки полностью разделены
        if (length(errorIndecies) == 0) break

        # выбираем случайный ошибочный объект          
        i = sample(errorIndecies, 1)
        xi = xl[i,]
        yi = classes[i]

        # высчитываем ошибку
        margin = sum(w * xi) * yi
        error = L(margin)

        # обновляем веса
        eta = 1 / iter
        w = updateRule(w, eta, xi, yi)

        # новое Q
        Q = (1 - lambda) * Q + lambda * error

        # выходим, если Q стабилизировалось
        if (abs(Q.prev - Q) / abs(max(Q.prev, Q)) < 1e-5) break

        # выходим, если слишком много итераций (алгоритм парализован)
        if (iter == 20000) break

        Q.prev = Q #запоминаем Q на предыдущем шаге
    }

    return(w)
}
```

Следующие алгоритмы за основу берут _стохастический градиент_, меняя только
функцию потерь
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D)
и правило обновления весов.

#### Адаптивный линейный элемент (adaline)

Имеет _квадратичную функцию потерь_
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29%3D%28M-1%29%5E2%3D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i-1%29%5E2)
и _дельта-правило_ правило обновления весов  
![](http://latex.codecogs.com/svg.latex?w%3Dw-%5Ceta%28%5Clangle%20w%2Cx_i%20%5Crangle-y_i%29x_i).



#### Персептрон Розенблатта

Имеет _кусочно-линейную функцию потерь_
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%3D%28-M%29_&plus;%3D%5Cmax%28-M%2C0%29)
и _правило Хебба_ для обновления весов
![](http://latex.codecogs.com/svg.latex?%5Ctext%7Bif%20%7D%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%3C0%20%5Ctext%7B%20then%20%7D%20w%3A%3Dw&plus;%5Ceta%20x_iy_i).

#### Логистическая регрессия

Также является __оптимальный байесовским классификатором__ из-за своих довольно
сильных вероятностных предположений.

Имеет _логистическую функцию потерь_
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29%20%3D%20%5Clog_2%281%20&plus;%20e%5E%7B-M%7D%29)
и _логистическое_ правило обновления весов
![](http://latex.codecogs.com/svg.latex?w%20%3A%3D%20w&plus;%5Ceta%20y_ix_i%5Csigma%28-%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)
, где
![](http://latex.codecogs.com/svg.latex?%5Csigma%28z%29%3D%5Cfrac%7B1%7D%7B1&plus;e%5E%7B-z%7D%7D)
– _сигмоидная функция_.

#### Вывод по линейным классификаторам



Рассмотрим примеры работы классификаторов

![](img/liniercl.png)


| Метод         | Параметра          | Количество ошибок (Loo) |
| ------------- |:------------------:| -----:|
|  <a href="src/2w.R">kNN</a>     | k = 4 , l = 150 |  0.04 |
|  <a href="src/Kwnn.r">Kwnn</a>  | k = 4 , l = 150 |  0.04 |
| Парзеновские окна|
| Ядро         | Параметры         | Количество ошибок (Loo) |
|  Прямоугольное      | h = 0.32 |  0.04 |
|  Треугольное   |  h = 0.32 |  0.04 |
|  Квартическое    |  h = 0.32 |  0.04 |
|  Епанечниково    |  h = 0.32 |  0.04 |
|  Гауссовское    |  h = 0.1 |  0.04 |


