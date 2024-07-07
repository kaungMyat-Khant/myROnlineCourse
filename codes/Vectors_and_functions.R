
# Atomic vectors are basis of the other R objects -------------------------

# Factors are built on top of integer vectors.
# Dates and date-time are built on top of numeric vectors.
# Data frames and tibbles are built on top of lists.
rm(list = ls())
factors <- factor(c("primary school","middle school","high school","graduate"),
                  levels = c("primary school","middle school","high school",
                             "graduate"), ordered = F)
str(factors)    
typeof(factors) # Type of vector is "integer"

dates <- as.Date(c("2024-04-20", "2024-04-21", "2024-04-22", "2024-04-23"), 
                 format = "%Y-%m-%d")
str(dates) 
typeof(dates) # Type of vector is "double" which is a numeric vector

df <- data.frame(factors, dates) ; df
str(df)
typeof(df) # Type of vector is "list"

tibbles <- tibble::tibble(factors, dates) ; tibbles
str(tibbles)
typeof(tibbles)

# Tibbles demand exact name when the columns are accessed, more strict than data frames.
# So tibbles are more robust in column name matching.
# But they are less versatile, less memory-efficient and less compatible with every R package.
# Tibble display the data better and can be aggregated

library(dplyr)
dfag <- df %>% group_by(factors) %>% count() ; dfag
str(dfag)

# When using tidy functions on data frames, aggregation or subset gives tibble
# So tibbles are more efficient with functions and programming
tdf <- table(df$factors) ; tdf 
str(tdf)
# And tibbles are more informative about data

rm(list = ls())

# Matrices ----------------------------------------------------------------

# Matrices are two dimensional objects of same vector type

matrix <- matrix(data = c(1:5, "a", "b"), nrow = 5, ncol = 2)
matrix 
str(matrix)
# Matrix coerces the vectors into character. 

matrix(data = 1:5, c("a","b")) # error
matrix <- matrix(data = list(x=1:5,y=c("a", "b")), nrow = 5, ncol = 2)
matrix
str(matrix)

# Matrix uses the list as a vector.

# Let's create two by two matrices
matrix1 <- matrix(1:4, nrow=2, ncol=2, byrow=T) ; matrix1
matrix2 <- matrix(1:4, nrow=2, ncol=2, byrow=F) ; matrix2

# Binding
matrix3 <- rbind(c("a","b","c"),1:3) ; matrix3
matrix4 <- cbind(c("a","b","c"),1:3) ; matrix4

rm(list = ls())
# Conditions --------------------------------------------------------------

1 == 1 # equal
1 != 2 # not equal
3 > 2 # greater than
2 < 3 # less than
3 >= 2 # greater than or equal
2 <= 3 # less than or equal
is.na(NA) # is missing
is.null(NULL) # is null
is.finite(0) # is a finite
is.infinite(Inf) # is infinite
is.integer(1L); is.character("a"); is.double(1.5)

# Selecting vector elements -----------------------------------------------

x <- c(-5:10,NA); x ; str(x)
# We use square bracket to select elements from vectors
# By position
x[5] # fifth element
x[-5] # except fifth element
x[2:5] # 2nd to 5th elements
x[-(2:5)] # except 2nd to 5th elements
x[c(1,17)] # first and last

# By value
x[x == 0] # value equal 0
x[x > 5] # values greater than 5
x[is.na(x)]
x[x %in% c(1,3,5,7,100)] #values in set {1,3,5,7,100}

# By name
y <- LETTERS; y; str(y)
y[y == "Z"] # element named "Z"
y[y == "z"] # there is no element named "z"

# We use [row, col] to select the element of a matrix
m <- matrix(1:9, nrow =3, ncol =3) ; m; str(m)
n <- matrix(1:9, nrow =3, ncol =3, byrow = T); n; str(n)

m[1,] # first row 
m[, 1] # first column
n[3,2] # third row, second column

# Transpose matrix
t(n)
m
t(n) == m
n == m

rm(list = ls())


# Functions ---------------------------------------------------------

x <- c(1,10,100,1000)
y <- c("A","A","B","B","A","C")
z <- 10

# Math function
log10(x) #log base 10
log(x) # natural log (ln/log base e)
exp(x) # exponential (reverse of ln)

x^2
format(x^2, scientific = F)
sprintf("%7d",x^2)
sprintf("%07d",x^2)

sqrt(x)

sum(x)
sum(c(x,NA))
sum(c(x,NA),na.rm = T)

mean(x)
mean(c(x,NA))
mean(c(x,NA), na.rm = T)
sd(x)
var(x)

median(x)
max(x)
min(x)
quantile(x)
rank(x)

sqrt(x)
round(sqrt(x),3) #round to 3 decimal places
signif(sqrt(x),3) #round to 3 significant number

cor(1:20,(1:20)^2)
plot(1:20,(1:20)^2)
cor(data.frame(1:20,(1:20)^2))

# Vector function
sort(x)
rev(sort(x))

y
table(y)
unique(y)
prop.table(table(y))

x+x #addition
x-x #subtraction
x*x #multiplication
x/x #division
x%%x #remainder
x%/%x #quotient
abs(-5) # absolute

# String functions
paste(x,y,sep="-")
paste0("ID",sprintf("%03d",1:100))
tolower(y)
toupper("y")

# Lists -------------------------------------------------------------------

l <- list(x = 1:3, y = c("a","b","c"), z = c(T,F,T))
l
str(l)
typeof(l)

# Lists are called recursive vectors. 
# Atomic vectors are homogenous.
# List are heterogenous.
# A list can contain other lists, suitable for hierarchial or tree-like structures.

# Sub-setting list can be confusing. We can use [],[[]] or $

l[x]; str(l[x])
l[2]; str(l[2])
l[1:2]; str(l[1:2])
l[2:3]; str(l[2:3])
# [] gives sub-list of the list as list. It's like pepper shaker containing pepper packets

l; str(l)
l$x; str(l$x)
l[["x"]]; str(l[["x"]])
l[[1]] ; str(l[[1]])
# [[]] and $ give components of the list as vector. It's like a pepper packet.

l[["z"]][2]; str(l[["z"]][2])
l[["z"]][[2]]; str(l[["z"]][[2]])
l[[2]][[1]]; str(l[[2]][[1]])
# [[]][[]] gives element of the sub-list of the list. It is pepper.
