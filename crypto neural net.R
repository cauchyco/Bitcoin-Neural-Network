set.seed(1000)

options(stringsAsFActors=F)
crypto = read.csv("cryptonumbers2017.csv", stringsAsFactors=F)
summary(crypto)

dim(crypto)
lapply(crypto, class)

View(crypto)

apply(crypto, 2, function(x) sum(is.na(x)))
crypto = crypto[, sapply(crypto, is.numeric)]

maxs = as.numeric(apply(crypto, 2, max))
mins = as.numeric(apply(crypto, 2, min))

scaled.data = as.data.frame(scale(crypto, center = mins, scale = maxs - mins))

print(head(scaled.data, 2))

cryptodata = sample(1:nrow(crypto), round(0.75*nrow(crypto)))

train1 = crypto[cryptodata,]
test1 = crypto[-cryptodata,]
lm.fit = glm(BTC~., data=train1)
pr.lm = predict(lm.fit, test1)
MSE.lm = sum((pr.lm - test1$BTC)^2)/nrow(test1)

train = scaled.data[cryptodata,]
test = scaled.data[-cryptodata,]


library(neuralnet)
n = names(train)
f = as.formula(paste("BTC ~", paste(n[!n %in% "BTC"], collapse = " + ")))
nn = neuralnet(f,data=train,hidden=c(5,3),linear.output=F)

plot(nn)

n.test = names(test)
f = as.formula(paste("BTC ~", paste(n[!n %in% "BTC"], collapse = " + ")))

pr.nn = compute(nn, test[,2:6])
show(pr.nn$net.result)
pr.nn_ = pr.nn$net.result*(max(crypto$BTC)-min(crypto$BTC))+min(crypto$BTC)
test.r = (test$BTC)*(max(crypto$BTC)-min(crypto$BTC))+min(crypto$BTC)

MSE.nn = sum((test.r - pr.nn_)^2)/nrow(test)

print(paste(MSE.lm,MSE.nn))

summary(train)
summary(test)

par(mfrow=c(1,2))

plot(test$BTC,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$BTC,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)



compute(nn, )
