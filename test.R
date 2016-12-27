data3 <- read.csv(file = "data/Lab3Task3Var6.csv")
head(data3)
data3$X <- NULL
head(data3)
length(data3$y)
sum(complete.cases(data3))

length(data3$y) == sum(complete.cases(data3))

percent <- 0.2
nas <- round(percent * N)

data3 <- apply(data3, 2, function (x) {
  x[sample(1:N)[1:nas]] <- NA
  x
})

length(data3$y) == sum(complete.cases(data3))