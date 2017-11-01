setwd("/Users/HanBo/Desktop/Courses/503/project/")
save.image("work0413.RData")
require(ggplot2)
require(gridExtra)
require(knockoff)
require(reshape2)
require(randomForest)
require(e1071)
library("VSURF")
require(reshape2)
require(sparsediscrim)
set.seed(123123)
require(GGally)
require(xgboost)



#data cleaning----
d1 <- read.table(file = "wdbc.data.txt",sep=",")
d2 <- read.table(file = "wpbc.data.txt",sep=",")
d2[,c(3,34,35)] <- NULL
d2$V2 <- "M"

tp=setdiff(d2$V1,d1$V1)
colnames(d1) <- 1:32
colnames(d2) <- 1:32
d <- rbind(d1,d2[tp,])

head(d1)
head(d2)
d <- rbind(d1,d2)
colname = colnames(d)
colnames(d) = 1:32

#knockoff variable selection----
vst <- knockoff.filter(X = d[,3:32],fdr=0.35,as.numeric(d[,2])-1,randomize = FALSE)
vst #explain 3 mean 7 worst

# heat map----
dd <- d[,3:32]
corr_mat = cor(dd)
# Remove upper triangle
corr_mat_lower = corr_mat
corr_mat_lower[upper.tri(corr_mat_lower)] = NA
# Melt correlation matrix and make sure order of factor variables is correct
corr_mat_melted = melt(corr_mat_lower)
corr_mat_melted$Var1 = factor(corr_mat_melted$Var1, levels=colnames(corr_mat))
corr_mat_melted$Var2 = factor(corr_mat_melted$Var2, levels=colnames(corr_mat))
# Plot
corr_plot = ggplot(corr_mat_melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white') +
  scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
                       name='Correlation') +
  ggtitle('Correlations') +
  coord_fixed(ratio=1) +
  theme_minimal() +
  scale_y_discrete(position="right") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        legend.position=c(0.1,0.9),
        legend.justification=c(0,1))
corr_plot

pp <- ggpairs(vs,data = )
pp

#FA----
factanal(d[,3:32],factors = 6,lower=0.01)
colname


# partition train and test----
set.seed(2017)
id <- sample(1:nrow(d),0.7*nrow(d))
tr <- d[id,] 
ts <- d[-id,]
table(tr$`2`)
table(ts$`2`)
mean(tr$`2`=='B')
mean(ts$`2`=='B')

# check balance
tt <- glmnet(pd_sca.x, pd_sca.y, alpha = 1,family = "binomial")
tt <- as.matrix(coef(cvtp, s = "lambda.min"))
iid <- which(tt!=0)-1+2
colname[iid]
mat[which(tt!=0)]=mat[which(tt!=0)]+1
# variable selection----
#LASSO
mat = matrix(0,ncol = 31)
N = 300
str(d)
pd_sca.x = as.matrix(scale(d[,3:32]))
pd_sca.y = as.factor(d[,2])
for (i in 1:N){
  if (i%%50==0){
    print(i)
  }
  tp = sample(1:nrow(d),0.7*nrow(d))
  x.tp = pd_sca.x[tp,]
  y.tp = pd_sca.y[tp]
  cvtp = cv.glmnet(x.tp, y.tp, alpha = 1,family = "binomial")
  tt <- as.matrix(coef(cvtp, s = "lambda.min"))
  mat[which(tt!=0)]=mat[which(tt!=0)]+1
}
df <- as.data.frame(t(mat))
df$id <- 1:31
varname = colnames(pd_sca.x)
df$names <- c("intercept",varname)
df$colname = c("inter",colname[3:32])
df$fre = df$V1/300
ddf <- df[order(df$V1,decreasing = T),]
ddf[2:13,c(4,5)]

#VSURF(pd_sca.x,y = pd_sca.y)

#tp = d[,2:32]
#pd_smo<-SMOTE(diagnosis~.,tp ,perc.over=100,perc.under=400)
#lapply(pd_smo,g)
#duplicated(tp)

colnames(vs)
ddf[2:14,4]

#KNOCKOFF----
set.seed(2015)
num = matrix(0,ncol = 30)
for (i in 1:N){
  if (i%%50==0){
    print(i)
  }
  tp = sample(1:nrow(d),0.7*nrow(d))
  x.tp = pd_sca.x[tp,]
  y.tp = pd_sca.y[tp]
  tt <- knockoff.filter(X = x.tp,fdr=0.3,as.numeric(y.tp)-1,randomize = FALSE)
  tt <- as.matrix(tt$selected)
  num[tt]=num[tt]+1
}

df2 <- as.data.frame(t(num))
df2$id <- 1:30
df2$names <- varname
df2$colname = c(colname[3:32])
df2$fre = df2$V1/300
ddf2 <- df2[order(df2$V1,decreasing = T),]
ddf2[,c(4,5)]

sd(mat)
sd(num)
ddf$cumsum <- cumsum(ddf$V1)
plot(ddf$cumsum)

d[,ddf[2:13,2]+2]
tmp <- d[,ddf[2:13,2]+2]
save(tmp,file = "vs.RData")




#lasso plot----
trace.plot = function(lambda.min = 0, lambda.max = 0.03, length.out = 1000, alpha = 1){
  beta = NULL
  for(lambda in seq(lambda.min, lambda.max, length.out = length.out)){
    fit = glmnet(pd_sca.x, as.numeric(pd_sca.y), alpha = alpha, lambda = lambda)

    beta = rbind(beta, fit$beta[, 1])
  }
  df22 = as.data.frame(beta)
  head(df22)
  
  df222 = melt(df22)
  df222$lambda = rep(-log(seq(lambda.min, lambda.max, length.out = length.out)), 30)
  
  
  
  p = ggplot(df222, aes(x = lambda, y = value, color = variable)) + 
    geom_line(aes(linetype = variable)) + theme_bw() + xlab("-log(lambda)")
  return(p)
}


k=trace.plot()
k


# svm ----


# random forest----
?randomForest
randomForest(formula=2~.,data = d[2:32])


# classfication--XGBoost----
require(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')

train <- agaricus.train
test <- agaricus.test
str(train)  
ttp <- as.matrix(train$data)

mx = 3:15
eta = c(0.1,0.3,0.5,0.8,1,1.2,1.5,1.7,1.9,2:7)
bstDense <- xgboost(data = as.matrix(tr[,3:32]), label = as.numeric(tr[,2])-1, max.depth = 5, eta = 1, nthread = 2, nround = 2, objective = "reg:logistic")
?xgboost
pred <- predict(bstDense, as.matrix(ts[,3:32]))
pdd <- round(pred)
mean(pdd == as.numeric(ts$diagnosis)-1)

mat <- matrix(0,nrow=length(mx), ncol =length(eta)) 
?matrix
for (i in 1:length(mx)){
  for(j in 1:length(eta)){
        bd <- xgboost(data = as.matrix(tr[,3:32]), 
                            label = as.numeric(tr[,2])-1, max.depth = mx[i], eta = eta[j], nthread = 2, nround = 2, objective = "binary:logistic")
        pred <- predict(bd, as.matrix(ts[,3:32]))
        pdd <- round(pred)
        mat[i,j] <- mean(pdd == as.numeric(ts$diagnosis)-1)
  }
}
mat

which(mat == max(mat), arr.ind = TRUE)

#best depth  best eta = 
mx[2]
eta[7]


nround = 4:20
max.depth = 1:8

subsample = seq(0.5,1,length.out = 10)
colsample = subsample


wi = seq(0.5,3,length.out = 5)

gamma = exp(-5:2)
eta = exp(-2:3)


mm <- matrix(0,nrow=length(nround))
for (i in 1:length(nround)){
  bd <- xgboost(data = as.matrix(tr[,3:32]), 
                label = as.numeric(tr[,2])-1, max.depth = 10, eta = 1, nthread = 4, nround = nround[i], objective = "binary:logistic")
  pred <- predict(bd, as.matrix(ts[,3:32]))
  pdd <- round(pred)
  mm[i] <- mean(pdd == as.numeric(ts$diagnosis)-1)
}
mm

bd <- xgboost(data = as.matrix(tr[,3:32]), 
              label = as.numeric(tr[,2])-1, max.depth = 10, eta = 1, gamma=0.1,nthread = 4, nround = 20, objective = "binary:logistic")
pred <- predict(bd, as.matrix(ts[,3:32]))
pdd <- round(pred)
mean(pdd == as.numeric(ts$diagnosis)-1)

sp = seq(0.5,1,length.out = 10)
for (i in 1:length(nround)){
  for(j in 1:length(eta)){
    bd <- xgboost(data = as.matrix(tr[,3:32]), 
                  label = as.numeric(tr[,2])-1, max.depth = mx[2], eta = eta[7], nthread = 2, subsample=1, nround = nround[j], objective = "binary:logistic")
    pred <- predict(bd, as.matrix(ts[,3:32]))
    pdd <- round(pred)
    mat[i,j] <- mean(pdd == as.numeric(ts$diagnosis)-1)
  }
}

bd <- xgboost(data = as.matrix(tr[,3:32]), 
              label = as.numeric(tr[,2])-1, max.depth = mx[2], eta = eta[7], nthread = 2, subsample=1, colsample=0.1,nround = nround[j], objective = "binary:logistic")
pred <- predict(bd, as.matrix(ts[,3:32]))
pdd <- round(pred)
mean(pdd == as.numeric(ts$diagnosis)-1)


#write a CV function
set.seed(2017)
bc = d
n = nrow(bc)
t = sample(1:n, 0.8 * n)
trainX = as.matrix(bc[t, -c(1,2)])
trainY = as.numeric(bc[t, 2]) - 1
testX = as.matrix(bc[-t, -c(1,2)])
testY = as.numeric(bc[-t, 2]) - 1

folds = cv_partition(1:nrow(tr), num_folds=5)
cv_xgb = function(trainX, trainY, testX, testY, folds, nround = 2) {
  cv_tot = 0
  train_tot = 0
  for(i in 1:5) {
    xgb = xgboost(data = trainX[folds[[i]]$train, ], label = trainY[folds[[i]]$train],
                  max.depth = 10, eta = 1, nthread = 4, 
                  nround = nround, objective = "binary:logistic")
    cv_tot = cv_tot + mean( round(predict(xgb, trainX[folds[[i]]$test, ])) 
                            != trainY[folds[[i]]$test])
  }
  xgb = xgboost(data = trainX, label = trainY,
                max.depth = 10, eta = 1, nthread = 4, 
                nround = nround, objective = "binary:logistic")
  train_err = mean( round(predict(xgb, trainX)) != trainY)
  test_err = mean(round(predict(xgb, testX)) != testY)
  data.frame(cv=cv_tot/5, train = train_err, test = test_err)
}



cv_xgb(trainX,trainY,testX,testY,folds = folds)

??cv_partition

ttp <- cv.glmnet(pd_sca.x, as.numeric(pd_sca.y))
tt <- as.matrix(coef(cvtp, s = "lambda.min"))
which(tt!=0)-1+2
colname[which(tt!=0)-1+2]
