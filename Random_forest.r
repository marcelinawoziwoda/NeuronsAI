library(party)
require(randomForest)
covtype <- read.csv('C:/Users/MarCy$/Desktop/STUDIA 6 semestr/sztuczna inteligencja/letter-recognition.data', header = FALSE)
summary(covtype)


covtype <- covtype[1:20000,c(1,3,6:16)]
covtype$V1 <- as.factor(covtype$V1)    
train<-sample(1:nrow(covtype), 7000)
covtype2 <- covtype[train,]
rf <- randomForest(V1 ~ ., data = covtype2, trees=600)
covtype1 <- covtype[-train,]
cm <- table(covtype1$V1, predict(rf, covtype1))


cm2 <- cm
err <- rep(0, nrow(cm2))

for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
cbind(cm2, err)
round(cbind(cm2, err),2)
#recognition rate
rr<- 0
for (a in 1:nrow(cm))
{
  rr <- rr + cm[a,a]
}
round(rr / sum(cm),4)


summ <- 0
for (a in 1:nrow(cm))
{
  summ<- summ + cm[a,a]
}
summ/sum(cm)

importance(rf)
print(rf)


