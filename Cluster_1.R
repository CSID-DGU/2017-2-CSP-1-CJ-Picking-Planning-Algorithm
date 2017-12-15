getwd()
setwd("c:/Users/xgh02/Desktop/7월데이터csv") #WORKING DIRECTORY 변경
install.packages("NbClust")
library(NbClust) #K값 분석을 위한 NbClust 패키지 호출
set.seed(123) #K-MEANS 결과값 고정을 위한 SEED 지정

#K-MEANS 분석에 앞서 K값의 최적값을 파악해봄
nc <- NbClust(test, min.nc=2, max.nc=15, method = "kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab = "Number of Criteria", 
        main = "Number of Clusters Chosen by 25 Criteria")

testData <- read.csv("0703_12차.csv") #파일 호출
str(testData)
head(testData)
colSums(is.na(testData)) #공백값 검사
testData <- na.omit(testData) #공백값 제거
testData

test <- testData[, c("SKU_CD", "SEQ")] #클러스터링을 위해 필요한 COLUMN만 분리
test
testData.result <- kmeans(test, 2, nstart=25) #K=2로 설정한 후 K-MEANS 분석 실행
testData.result

cluster <- data.frame(testData$SKU_NM, testData.result$cluster)
cluster2 <- unique(cluster)
cluster2
plot(test$SKU_CD, test$SEQ, type="n",xlab="SKU_CD",ylab="SEQ") #데이터 시각화
text(x=test$SKU_CD, y=test$SEQ, labels=cluster2$SKU_NM, col=testData.result$cluster+1)








