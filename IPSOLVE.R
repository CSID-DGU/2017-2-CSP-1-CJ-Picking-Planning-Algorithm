library(lpSolve)
library(lpSolveAPI)
library(xlsx)

getwd()
setwd("C:/Users/xgh02/Desktop")

csv <- read.csv("0629_9Â÷.csv", header = T)
csv2 <- read.csv("sample2.csv", header = T)
str(csv)
csv <- na.omit(csv)

csv.edit <- 
csv.edit <- data.frame(csv$SKU_CD, csv$SKU_NM, csv$PICKCNT)
colnames(csv.edit) = c("SKU_CD", "SKU_NM", "PICKCNT")

freq.sku <- data.frame(table(csv$SKU_NM))
colnames(freq.sku) = c("SKU_NM", "FREQ")
freq.sku

amount.sku <- data.frame(aggregate(PICKCNT~SKU_NM, csv, sum))
amount.sku

table <- merge(freq.sku, amount.sku, by = "SKU_NM")
table

write.xlsx(table, file="table.xlsx", 
           sheetName="0629_9", col.names=TRUE, row.names=FALSE, append=TRUE)
csv2
sku.freq2 <- csv2$FREQ
sku.amount2 <- csv2$PICKCNT
sku.freq <- c(117, 96, 31, 90, 10, 48, 50,
              5, 12, 5, 16, 3, 71, 5, 10,
              3, 4, 4, 143, 5, 5, 37, 28,
              1, 4, 9, 8, 7, 2, 23, 70, 2)

sku.amount <- c(209, 329, 156, 227, 112,
                116, 125, 10, 19, 6, 46,
                6, 87, 6, 81, 7, 6, 10,
                237, 8, 10, 84, 50, 3,
                6, 27, 49, 34, 11, 33,
                488, 4)

sku.weight <- c(630, 80, 500, 500, 2700,
                36, 2000, 100, 900, 200,
                150, 455, 320, 900, 167,
                500, 7200, 500, 280, 210,
                500, 60, 180, 1000, 460,
                27, 900, 130, 27, 500,
                2000, 340)

selves.weight <- c(9, 5, 1, 13, 10, 6, 2, 14, 11, 7, 3, 15, 12, 8, 4, 16,
                   9, 5, 1, 13, 10, 6, 2, 14, 11, 7, 3, 15, 12, 8, 4, 16)

vari.f <- 0

for(i in 1:1024){
  vari.f[i] <- 1
}

for(i in 1:32){
  for(j in 1:32){
    vari.f[ 32 * (i-1) + j] <- 
      vari.f[ 32 * (i-1) + j] * ( sku.amount2[i] / sku.freq2[i] ) * selves.weight[j] * sku.weight[i]
  }
}

rhs <- 0
for(i in 1:64){
  rhs[i] <- c(1)
}

constr.dir <- c(rep("=", 64))
obj.fun.double <- vari.f

constr <- matrix(0, 64, 1024)

m <- 32
n <- 32

for(i in 1:m){
  for(j in 1:n){
    constr[i, j + (i-1)*32] <- 1
    constr[32+i, (i-1) + 32*j - 31] <- 1
  }
}

prod.model <- lp("min", obj.fun.double, constr, constr.dir, rhs, compute.sens = TRUE)
prod.model$objval
prod.model.sol <- matrix(prod.model$solution, 32, 32)
prod.model.sol

write.xlsx(prod.model.sol, file="sample.xlsx", 
           sheetName="third", col.names=TRUE, row.names=FALSE, append=TRUE)
