getwd()
setwd("c:/Users/xgh02/Desktop/7월데이터csv")


install.packages('readxl')
library(readxl)
install.packages('xlsx')
install.packages('rJava')
require(xlsx)
require(rJava)
require(readxl)


cv <- read.csv("0731_12차.csv", header=TRUE)

csv_ver <- data.frame(cv$SKU_CD,cv$SKU_NM)
freq_csv <- table(csv_ver$cv.SKU_NM)
sort_csv <- sort(freq_csv)
sort_csv <- data.frame(sort_csv)
colnames(sort_csv) <- c("SKU_NM", "FREQ")

write.xlsx(sort_csv, file="0731.xlsx", 
           sheetName="12차", col.names=TRUE, row.names=FALSE, append=TRUE)
