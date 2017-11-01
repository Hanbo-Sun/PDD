pdf('./heatmap.pdf', height=6, width=7.5)
corr_plot
dev.off()

save(corr_plot,file="corpl.RData")

vs = data.frame(vs,diagnosis=d$diagnosis)

ggpairs(vs,
        mapping=aes(color=diagnosis),
        axisLabels = "internal",
        upper=list(continuous='points'))

#svm----
set.seed(2015)
id = sample(1:628, 0.6*628)
trainX =  sc[id,3:32]
trainY = sc[id,2]
testX = sc[-id,3:32]
testY = sc[-id,2]

trainX =  lg[id,3:32]
trainY = lg[id,2]
testX = lg[-id,3:32]
testY = lg[-id,2]


vs = data.frame(vs,diagnosis=d$diagnosis)
trainX =  vs[id,1:12]
trainY = vs[id,13]
testX = vs[-id,1:12]
testY = vs[-id,13]

bc_svm_tune1 <- svm(trainX, trainY, kernel = "radial", cost = 2, gamma =0.0078125)
bc_svm_tune1 <- svm(trainX, trainY, kernel = "radial", cost = 2, gamma =0.0078125,probability = T)
pred <- predict(bc_svm_tune1, testX,probability = T)
pred

ttpp <- as.matrix(attr(pred,"probabilities"))
ttpp
pred <- ttpp[,"M"]


mean(testY == predict(bc_svm_tune1, testX))
mean(predict(bc_svm_tune1, testX)[testY=='M']=='M')

table(testY, predict(bc_svm_tune1, testX))

bc_svm_tune2 <- svm(trainX, trainY, kernel = "polynomial", cost =8, gamma = 0.125,
                    coef0 = 2, degree = 1)
bc_svm_tune2 <- svm(trainX, trainY, kernel = "polynomial", cost =8, gamma = 0.125,
                    coef0 = 2, degree = 1,probability = T)
pred <- predict(bc_svm_tune2, testX,probability = T)
mean(testY == predict(bc_svm_tune2, testX))
table(testY, predict(bc_svm_tune2, testX))
mean(predict(bc_svm_tune2, testX)[testY=='M']=='M')

set.seed(129)
id = sample(1:628, 0.6*628)
trainX =  sc[id,3:32]
trainY = sc[id,2]
testX = sc[-id,3:32]
testY = sc[-id,2]
df = data.frame(trainX,diagnosis=trainY)


trainX =  lg[id,3:32]
trainY = lg[id,2]
testX = lg[-id,3:32]
testY = lg[-id,2]
df = data.frame(trainX,diagnosis=trainY)


vs = data.frame(vs,diagnosis=d$diagnosis)
trainX =  vs[id,1:12]
trainY = vs[id,13]
testX = vs[-id,1:12]
testY = vs[-id,13]
df = data.frame(trainX,diagnosis=trainY)

rf <- randomForest(formula=diagnosis~.,data = df,mtree=1000,mtry=9)
mean(testY == predict(rf, testX))
mean(predict(rf, testX)[testY=='M']=='M')
head(testY)
head(predict(rf, testX))
#xgboost----
set.seed(2014)
set.seed(2019)

id = sample(1:628, 0.6*628)
trainX =  sc[id,3:32]
trainY = sc[id,2]
testX = sc[-id,3:32]
testY = sc[-id,2]

trainX =  lg[id,3:32]
trainY = lg[id,2]
testX = lg[-id,3:32]
testY = lg[-id,2]


vs = data.frame(vs,diagnosis=d$diagnosis)
trainX =  vs[id,1:12]
trainY = vs[id,13]
testX = vs[-id,1:12]
testY = vs[-id,13]
bstDense <- xgboost(data = as.matrix(trainX), label = as.numeric(trainY)-1, max.depth = 5, 
                    eta = 1, nthread = 2, nround = 18,booster="gblinear", gamma=0.1,
                    objective = "binary:logistic")
mean(as.numeric(testY)-1 == round(predict(bstDense, as.matrix(testX))))
pred <- round(predict(bstDense, as.matrix(testX)))
pred <- predict(bstDense, as.matrix(testX))




tt <- as.numeric(testY)-1 # M==1
head(testY)
head(tt)
mean(pred[tt==1]==1)

bstDense <- xgboost(data = as.matrix(trainX), label = as.numeric(trainY)-1, max.depth = 5, 
                    eta = 1, nthread = 2, nround = 18,booster="gbtree",
                    objective = "binary:logistic")
pred <- round(predict(bstDense, as.matrix(testX)))
tt <- as.numeric(testY)-1 # M==1
head(testY)
head(tt)
mean(pred[tt==1]==1)
pred4 <- predict(bstDense, as.matrix(testX))


