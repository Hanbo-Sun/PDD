library(pROC)
library(plotROC)
#devtools::install_github("sachsmc/plotROC")
library(ggplot2)

roc_obj <- roc(testY, pred1)
roc_obj$auc
auc(roc_obj)

0.9862
0.9862
0.9855
0.9803


ttp = as.numeric(testY)-1
dt <- data.frame(D = ttp, M1 = pred1,M2=pred2,M3=pred3,M4=pred4)

dt <- data.frame(M1 = pred1,M2=pred2,M3=pred3,M4=pred4)
require(reshape2)
head(dt)
dt <- melt(dt,id.vars = "D")
head(dt)


basicplot <- ggplot(dt, aes(d = D, m = value, colour=variable)) + geom_roc(labels = FALSE, size = 0.5, alpha = 0.6, linejoin = "mitre") +  
  theme_bw() + coord_fixed(ratio = 1) + style_roc() + 
  annotate("rect", xmin = 0.4, xmax = 1, ymin = 0.35, ymax = 0.65,
           alpha = .2)+
  annotate("text", x = 0.7, y = 0.5, size = 4,
           label = "SVM_Gaussian : 0.9922 \n SVM_Polynomial : 0.9947 \n XGB_Linear: 0.9924 \n XGB_TREE : 0.9865")
basicplot

pdf('./rocsc.pdf', height=6, width=6, family='Times New Roman')
plot.roc.sc 
dev.off()

save(dt,file="vs_df.RData")



