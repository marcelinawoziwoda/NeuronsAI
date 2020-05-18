dane <- read.csv(file='C:/Users/MarCy$/Desktop/STUDIA 6 semestr/sztuczna inteligencja/magic04.data',header = FALSE)

iris <- dane

summary(iris)

colnames(iris)<-c('fLength', 'fWidth', 'fSize', 'fConc', 'fConc1', 'fAsym', 'fM3Long', 'fM3Trans', 'fAlpha', 'fDist', 'class')

table(iris$class)

levels(iris$class)

iris$class <- factor(iris$class)

levels(iris$class)

library(neuralnet)

size.sample<-floor(3/4*nrow(iris))
size.sample <-50

set.seed(101)

samples_id<-sample(1:nrow(iris), size.sample)

iristrain<-iris[c(samples_id),]

irisvalidation <- iris[-c(samples_id),]

nnet_iristrain<-iristrain

nnet_iristrain$g <- iristrain$class == 'g'

nnet_iristrain$h <- iristrain$class == 'h'

nn <- neuralnet(g+h ~
                  fLength+fWidth+fSize+fConc+fConc1+fAsym+fM3Long+fM3Trans+fAlpha+fDist,
                data = nnet_iristrain,
                hidden=c(3), stepmax = 1e+06) #trzeba zmieniac liczbe

plot(nn)

mypredict <- compute(nn, irisvalidation[-c(11)])$net.result
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}


idx <- apply(mypredict, c(1), maxidx)
prediction <- c('g', 'h')[idx]

pred <- table(prediction, irisvalidation$class)

A<-as.matrix(pred)
pred
sum(diag(A))/sum(A)

