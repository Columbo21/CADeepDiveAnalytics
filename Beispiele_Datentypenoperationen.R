

age <- 1:10L
name <- c("Dominik", "Peter", "Hans")
income <- c(35000, 20000, 30000)
shoes <- c(45.3, 46, 33.1)
shoes <- seq(30, 31, 0.5)
sort (income)
summary (income)
table (income)
rev(income)
income [2]
age2 <- c(5,3,78,1)
age3 <- 10:12
mat <- matrix(income,nrow=3)
mat[2]
mat[,2]
list1 <- list(test=c("test", "test2"), income=income, age=age2)
list1$age
list1$income
df1 <- data.frame(name, income, shoes, age3)
df1[2,3]
df1[2,c(2,3)]
