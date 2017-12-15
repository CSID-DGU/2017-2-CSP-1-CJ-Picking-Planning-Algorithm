#0623_6차
library(NbClust) 
library(xlsx)
set.seed(1)
testData <- read.csv("0629_9차_1.csv") #파일 호출
str(testData)
head(testData)
testData
test <- testData[, c("SKU_CD", "SEQ")]
test2 <- testData[, c("SKU_CD", "SEQ", "SKU_NM")]
test
colSums(is.na(test))
colSums(is.na(test2)) #공백값 검사
test <- na.omit(test)
test2 <- na.omit(test2) #공백값 제거
test
nc <- NbClust(test, min.nc=2, max.nc=15, method = "kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab = "Number of Criteria", 
        main = "NbClust Barplot")

testData.result <- kmeans(test2[,c("SKU_CD", "SEQ")], 2, nstart=25) #K=2로 설정한 후 K-MEANS 분석 실행
testData.result
testData.result$size

cluster <- data.frame(test2$SKU_NM, testData.result$cluster)
cluster
cluster2 <- unique(cluster)
cluster2
range(test$SKU_CD)
plot(test2$SKU_CD, test2$SEQ, type="n",xlab="SKU_CD",ylab="SEQ", main = "0629_cluster") #데이터 시각화
points(x=test2$SKU_CD, y=test2$SEQ, col=testData.result$cluster+1)

cls1 <- cluster[cluster$testData.result.cluster == 1,]
cls2 <- cluster[cluster$testData.result.cluster == 2,]
cls1 <- unique(cls1)
cls2 <- unique(cls2)
cls1
write.xlsx(cls2, file="0629_9차_clu.xlsx", 
           sheetName="clu2", col.names=TRUE, row.names=FALSE, append=TRUE)
