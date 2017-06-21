library(ggplot2)
library(glmnet)
library(reshape2)
## 参考：http://xccds1977.blogspot.com/2012/05/glmnetlasso.html
set.seed(123456)
# 模拟数据
data <- data.frame(V1=rnorm(1000),V2=rnorm(1000))
data$V3=ifelse((data$V2^2+data$V1^2)>1,1,0)
# 模拟的数据，第三列是离散性的因变量，取决于第一列和第二列两个自变量。
# 主要判断该点距离原点c(0,0)距离是否超过1，所以下面的回归会用逻辑回归

## 

# 散点图
p=ggplot()+ 
  geom_point(data=data,aes(V1,V2,colour=factor(V3),
                           shape=factor(V3)),size=3)
print(p)
# 建立六阶多项式自变量
attach(data)
degree = 6
X = matrix(rep(1,length(V1)),ncol=1)
for (i in 1:degree) {
  for (j in 0:i) {
    X <-cbind(X, (V1^(i-j))*V2^j)
  }
}
x<- X[,-1]
## 本来自变量只有2个，通过一个六阶多项式增加了26个干扰自变量。
summary(x)
## 最终根据这28个自变量来进行LASSO模型的构建，来预测因变量Y。
Y <- data$V3

lm.sol <- lm(Y ~ x )
summary(lm.sol)
#利用car包中的vif（）函数
#library(car)
#vif(lm.sol)
plot(x[,1] ~ x[,4], col = "red"   )


# 用glmnet包建模
model <- cv.glmnet(x,Y,family="binomial",type.measure="deviance")
# 绘制CV曲线图，选择最佳lambda值
plot(model)
model$lambda.1se
# 提取最终模型
model.final <- model$glmnet.fit
# 取得简洁模型的参数系数
model.coef <- coef(model$glmnet.fit, s = model$lambda.1se)
# 取得原始模型的参数系数
all.coef <- coef(model$glmnet.fit, s =  min(model.final$lambda))

# 可以用predict进行预测
pre <-predict(model.final,newx=x,s=model$lambda.1se,type='class')
table(Y,pre)
# 预测结果太好了一点，因为这里训练集等于测试集。


# 下面的工作全部是为了绘制决策边界
 
u <- seq(-1,1.2, len=200)
v <- seq(-1,1.2, len=200)

z28 <-z9  <- matrix(0, length(u), length(v))

mapFeature <- function(u,v, degree=6) {
  out <- sapply(0:degree,function(i)
    sapply(0:i, function(j)
      u^(i-j) * v^j
    )
  )
  out <- unlist(out)
  return(out)
}

for (i in 1:length(u)) {
  for (j in 1:length(v)) {
    features <- mapFeature(u[i],v[j])
    z9[i,j] <- as.numeric(features %*% model.coef)
    z28[i,j] <- as.numeric(features %*%all.coef)
  }
}

rownames(z9) <- rownames(z28) <- as.character(u)
colnames(z9) <- colnames(z28) <-  as.character(v)

z9.melted <- melt(z9)
z28.melted <- melt(z28)
z9.melted <- data.frame(z9.melted, lambda=9)
z28.melted <- data.frame(z28.melted, lambda=28)

zz <- rbind(z9.melted, z28.melted)
zz$lambda <- factor(zz$lambda)
colnames(zz) <- c("u", "v", "z", "lambda")

p <- ggplot()+ 
  geom_point(data=data,aes(V1,V2,colour=factor(V3),shape=factor(V3)),size=3) +
  geom_contour(data=zz, aes(u, v, z = z,
                            group=lambda, colour=lambda),bins=1,size=1)

p










