# SMPR
<p><a href="#Vvonyye_opredeleniya">1. Вводные определения</a></p>
  <p><a href="#Metricheskiye_algoritmy">2. Метрические алгоритмы</a></p>
  <p><a href="#a1NN">2.1. 1NN</a></p>
  <p><a href="#akNN">2.2. kNN</a></p>
  <p><a href="#aLOO">2.3. LOO</a></p>
  <p><a href="#akwNN">2.4. kwNN</a></p>
  <p><a href="#aPW">2.5. Парзеновские окна</a></p>
  
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


| Метод         | Параметра          | Количество ошибок (Loo) |
| ------------- |:------------------:| -----:|
|  <a href="src/2w.R">kNN</a>     | k = 4 , l = 150 |  0.02 |
|  <a href="src/Kwnn.r">Kwnn</a>  | k = 4 , l = 150 |  0.04 |

### 2.4. PW(Парзеновские окна) <a name="aPw"></a>
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

Параметр ширины `h` раасчитывается с помощью `Loo` :

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





| Ядро         | Параметры         | Количество ошибок (Loo) |
| ------------- |:------------------:|       -----:|
|  Прямоугольное      | h = 0.32 |  0.04 |
|  Треугольное   |  h = 0.32 |  0.04 |
|  Квартическое    |  h = 0.32 |  0.04 |
|  Епанечниково    |  h = 0.32 |  0.04 |
|  Гауссовское    |  h = 0.1 |  0.04 |


