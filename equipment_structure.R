setwd("C:/r_temp/GSdata/structure")
orig_data <- read.csv("equipment structure.csv", header =T, sep =",")
orig_data$Missing <-0
orig_data$Missing[is.na(orig_data$Misc)] <-1
orig_data$logVolume <- log(orig_data$Volume)
orig_data$logMain <- log(orig_data$Main)


fit <- lm(logMain ~ logVolume , data = orig_data)
fit <- lm(Main ~ Volume + I(Volume^2), data = orig_data[-c(100,101),])
fit <- lm(Main ~ Volume + Missing, data = orig_data[-c(100,101),])
fit <- lm(Main ~ Volume , data = orig_data[-c(47,100,101),])
#hat <- hatvalues(fit)
#head(sort(hat, decreasing =T),20)

library(caret)
library(tree)

### pre-processing
data <- orig_data[-c(100, 101),]
set.seed(1)
train <- createDataPartition(data$Main, p= 0.7, list = FALSE)
log_tr <- createDataPartition(orig_data$logMain, p= 0.7, list = FALSE)

#### ----변수가 Main 하나일 때 -----------
### 단순선형회귀 
lm.fit <- lm(Main ~ Volume, data = data[train, ])
yhat <- predict(lm.fit, data.frame(Volume = data[-train, "Volume"]))
sqrt(mean((yhat - data$Main[-train])^2))

#### log 단순선형회귀 
log.lm.fit <- lm(logMain ~ logVolume, data = orig_data[log_tr, ])
yhat <- exp(predict(log.lm.fit, data.frame(logVolume = orig_data[-log_tr, "logVolume"])))
#yhat <- exp(predict(log.lm.fit, data.frame(logVolume = orig_data[-log_tr, "logVolume"])) +(summary(log.lm.fit)$sigma^2)/2)
sqrt(mean((yhat - orig_data$Main[-log_tr])^2))

plot(Main ~ Volume, data =orig_data[-log_tr,])
lines(sort(orig_data[-log_tr,]$Volume), sort(yhat))
plot(yhat, orig_data$Main[-log_tr])
abline(0,1)

### Tree
tree.structure <- tree(Main ~ Volume, data = data[train, ])
summary(tree.structure)
plot(tree.structure)
text(tree.structure, pretty = 0)
####--------

yhat <- predict(tree.structure, newdata = data[-train,])
sqrt(mean((yhat - data$Main[-train])^2))

library(DMwR2)
rt.str <- rpartXse(Main ~ Volume, data = data)
yhat <- predict(rt.str, data[-train,])
sqrt(mean((yhat - data$Main[-train])^2))

library(rpart.plot)
prp(rt.str, extra = 101, box.col = "orange", split.box.col = "grey")

### 능형 회귀
library(glmnet)
grid <- 10^seq(10, -2, length =100)
x <- model.matrix(Main ~ Volume, data)[,-1]
x <- t(t(x))
y <- data$Main
ridge.str <- glmnet(x[train], y[train], alpha = 0, lambda = grid, thresh =1e-12)
