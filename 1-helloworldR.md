# Hello World of R



## 1. (Вектор) Создать векторы `v1` и `v2`:

Вектор  `v1`  состоит  из  последовательных  членов  арифметической  прогрессии.  Первый  член прогрессии равен вашему номеру в списке группы, последний – числу `6.5`, а количество элементов последовательности – длине вашей фамилии.  

Let's create initial values

```r
surname <- 'PAULAU'
listNumber <- 4
```

And create `v1` vector as described above.

```r
v1 <- seq(listNumber, 6.5, length.out = nchar(surname))
print(v1)
```

```
## [1] 4.0 4.5 5.0 5.5 6.0 6.5
```

Вектор  `v2`  состоит  из  последовательных  членов  геометрической  прогрессии.  Первый  член прогрессии равен `14`  минус  ваш  номер  в  списке  группы,  последний  –  числу  `100`,  а  количество элементов последовательности – длине вашего имени. 

```r
v2 <- seq(14 - listNumber, 100, length.out = nchar(surname))
print(v2)
```

```
## [1]  10  28  46  64  82 100
```

Вывести вектор `v3` длины `3`, состоящий из случайно выбранных элементов векторов `v1` и `v2`.

```r
v3 <- sample(c(v1, v2), 3)
print(v3)
```

```
## [1] 10.0  5.5  6.5
```

## 2. (Вектор) Задана некоторая строка текста, состоящая только из строчных символов русского языка. Например `s <- “приветмирр”`.
Let's introduce alphabet entity that will help with getting letter's number

```r
create.alphabet <- function(string) {
  alphabet <- strsplit(string, c())[[1]]
  
  letter <- function(number) {
    alphabet[number]
  }
  
  number <- function (letter) {
    number <- grep(letter, alphabet)
    ifelse(length(number) == 0, 0, number)
  }
  
  assignNumbers <- function(string) {
    sapply(strsplit(string, c())[[1]], number)
  }
  
  assignLetters <- function(numbers) {
    sapply(numbers, letter)
  }

  list(assignNumbers = assignNumbers,
       assignLetters = assignLetters,
       length = length(alphabet))
}

alphabet <- create.alphabet('абвгдеёжзийклмнопрстуфхцчшщъыьэюя')
```

<!--Enter string:

-->
### __1 вариант.__ Найти сумму порядковых номеров в алфавите символов строки `s`. Нумерация букв алфавита начинается с `1`. Например, для `s = “абба”` ответ должен быть 6. 

```r
s <- 'абба'
print(alphabet$assignNumbers(s))
```

```
## а б б а 
## 1 2 2 1
```

```r
print(sum(alphabet$assignNumbers(s)))
```

```
## [1] 6
```

### __2 вариант.__ Найти  произведение  тех  порядковых  номеров  в  алфавите  символов  строки  `s`, номера которых нечетные. Нумерация букв алфавита начинается с `1`. Например, для `s = “аббв”` ответ должен быть `3`. 

```r
s <- 'аббв'
oddChars <- alphabet$assignNumbers(s)[c(TRUE, FALSE)]
print(oddChars)
```

```
## а б 
## 1 2
```

```r
print(prod(oddChars))
```

```
## [1] 2
```

### __3 вариант.__  Зашифровать  текст  s  с  помощью  шифра  Цезаря.  Например,  для  s  =  “абба”  ответ должен быть “гддг”.

```r
s <- 'абба'

create.ceasar <- function(key) {
  alphabet <- create.alphabet('абвгдеёжзийклмнопрстуфхцчшщъыьэюя')
  
  shift <- function(number) {
    (number + key) %% alphabet$length
  }
  
  unshift <- function(number) {
    (number - key) %% alphabet$length
  }
  
  setKey <- function(number) {
    key <<- number
  }
  
  code <- function(string) {
    paste(alphabet$assignLetters(shift(alphabet$assignNumbers(string))), collapse = '')
  }
  
  encode <- function(string) {
    paste(alphabet$assignLetters(unshift(alphabet$assignNumbers(string))), collapse = '')
  }
  
  list(setKey = setKey, code = code, encode = encode)
}

scrambler <- create.ceasar(3)

print(scrambler$code(s))
```

```
## [1] "гддг"
```

```r
print(scrambler$encode(scrambler$code(s)))
```

```
## [1] "абба"
```

## 3. (Матрица) Создать квадратную матрицу `m` размером 5 x 5.
Заполнить ее элементами вектора `v1` построчно, если первый элемент `v3` больше `10`, иначе по столбцам. Если длины `v1` не хватает, то пусть элементы этого вектора повторяются.

```r
m <- matrix(v1, 5, 5, v3[1] > 10)
```

```
## Warning in matrix(v1, 5, 5, v3[1] > 10): data length [6] is not a sub-
## multiple or multiple of the number of rows [5]
```

```r
print(m)
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]  4.0  6.5  6.0  5.5  5.0
## [2,]  4.5  4.0  6.5  6.0  5.5
## [3,]  5.0  4.5  4.0  6.5  6.0
## [4,]  5.5  5.0  4.5  4.0  6.5
## [5,]  6.0  5.5  5.0  4.5  4.0
```

Присвоить столбцам имена следующим образом: если `N` – это ваш номер по списку, то `n` равно остатку от деления `N` на `12` плюс `1`, тогда первый столбец имеет имя n-го месяца на английском языке, второй – `(n+1)`-го и т.д. Строки присвоить такие же имена  как  и  столбцам.

```r
start <- listNumber %% 12 + 1
rownames(m) <- month.name[start : (start + 4)] -> colnames(m)
print(m)
```

```
##           May June July August September
## May       4.0  6.5  6.0    5.5       5.0
## June      4.5  4.0  6.5    6.0       5.5
## July      5.0  4.5  4.0    6.5       6.0
## August    5.5  5.0  4.5    4.0       6.5
## September 6.0  5.5  5.0    4.5       4.0
```

Матрица  `m1`  получается  из  матрицы  `m`  путем  вычеркивания  строк  и столбцов, чьи имена начинаются на буквы от `A` до `F`.

```r
m1 <- m[-grep('^[A-F]', rownames(m)), -grep('^[A-F]', colnames(m))]
```

Для матрицы `m1` найти: определитель, собственные вектора и значения, вектор диагональных элементов, $m1^2$ и матрицу `m2`, у которой $v2[i][j]$ = $(v1[i][j])^2$

Determinant

```r
print(det(m1))
```

```
## [1] -137.25
```

Eigen values

```r
print(eigen(m1)$values)
```

```
## [1] 20.477355+0.00000i -1.488677+1.50073i -1.488677-1.50073i
## [4] -1.500000+0.00000i
```

Diagonal

```r
print(diag(m1))
```

```
##       May      June      July September 
##         4         4         4         4
```

Squared

```r
print(m1 %*% m1)
```

```
##              May   June   July September
## May       105.25 106.50 115.25    111.75
## June      101.50 104.75 106.50    105.50
## July       96.25 101.50 105.25     97.75
## September  97.75 105.50 111.75    106.25
```


```r
m2 <- m1 * m1
print(m2)
```

```
##             May  June  July September
## May       16.00 42.25 36.00     25.00
## June      20.25 16.00 42.25     30.25
## July      25.00 20.25 16.00     36.00
## September 36.00 30.25 25.00     16.00
```

## 4. Создать произвольную таблицу данных (Dataframe)
В не будут присутствовать данные следующих  типов:  числовые,  текстовые,  условные,  факторы.

```r
df <- data.frame(number=sample(1:10), text=sample(month.name[1:10]), bool=sample(rep(c(FALSE, TRUE), 5)), fact=as.factor(sample(rep(c('A', 'B'), 5))))
```

Вывести  все  такие  текстовые значения для заданного фактора, для которых числовые значения больше заданного значения.

```r
df[df$fact == "A" & df$number > 2, ]
```

```
##    number    text  bool fact
## 5       4  August  TRUE    A
## 6      10   April FALSE    A
## 7       8    July  TRUE    A
## 10      6 October FALSE    A
```

## 5. (Таблица) ИЗ файла загрузить таблицу, состоящую из трех переменных (x1, x2, x3) (переменная = столбец) и n наблюдений (строк).
