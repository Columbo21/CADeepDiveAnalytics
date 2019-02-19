library(magrittr)


# atomic data types
# *********************

num <- 5.1 # numeric
int <- 5L # integer
char <- '433' # character
char2 <- "slkjs" # character

logic <- TRUE # logical
logic2 <- TRUE | FALSE # logical
logic3 <- T #logical

date <- as.Date('2015-01-01')

null <-NULL 
na <- NA

# testing for data types

null > 5
na > 5

is.numeric(num) # TRUE
is.integer(num) # FALSE

is.numeric(int) # TRUE
is.integer(int) # TRUE

is.character(char)

is.logical(F)

# Data constructs
# *********************

# Vectors
c(num, char)
c(num, int)
c(logic, num)
c(logic, int)

vec <- c(num, int, logic, char)

# Calculation with vectors

x <- 1:10
y <- 5:14

x + y

x * y
x / y

# Recycling
y <- 1:2

x + y
x * y

# Indexing

x[5] # the fifth element
x[5:10] # the fifth to tenth

x[x < 5] # logical

ind <- x < 4 | x > 9
x[ind]

x[c(T, F, F)]

x[8:10] <- NA

seq(5, 100, by = 5)
1:20 * 5

rep(1:5, 3)

# Matrix

mat <- matrix(1:9, nrow=3) # numeric matrix
mat <- matrix(letters, nrow=2) # character matrix
mat[2, 5:10]

# Lists

l <- list(first = x, second = y, third = char) # named list
 
names(l)

l$first

l[1] # first list element (still list)

l[[1]][5] # the fifth vector element of the first list element

a <- list()

a$nice <- function(x) sum(x)
a$other <- 5


# Dataframes

names <- c('Henri', 'Klaus', 'Jürgen')
age <- c(3, 21 , 55)
isadult <- age > 18

df <- data.frame(names, age, isadult)

df$age
df['age']
df[['age']]
df[,2]
df[1,1] <- 'Jochen'

str(df)
summary(df)

