setwd("/Users/adrianhsu/Google_Drive/NTUEE_106_1/sta/sta2017/0929-lab1")

dat<-read.table('pheno_com.txt', header=TRUE, row.names = 1)
newdat<-cbind(dat$age, dat$AD, dat$DDAST)
newdat1<-as.data.frame(newdat) #轉成 data.frame的指令
colnames(newdat1)<-c("age", "AD", "DDAST")

# 敘述統計
sum(newdat1$age)

# max min median mean...
var(newdat1$age)
sd(newdat1$age)
sum(newdat1$age) / length(newdat1$age)
quantile(newdat1$age)
quantile(newdat1$age, probs=c(0, 1/3, 2/3, 1))
quantile(newdat1$age, probs=seq(0, 1,0.1))

apply(newdat1, 2, mean) # 2: column vector, 共3個mean
apply(newdat1, 1, mean) # 1: row vector, 共330個mean

apply(newdat1, 2, quantile)

apply(newdat1, 2, function(x) quantile(x,probs=seq(0,1,0.1))) # 自定義function

# lapply, sapply
# 用在：拿age當comparator, 其他column就跟著sort
sort(newdat1$age) # 從小到大排，其實就跟order順序一樣
ord<-order(newdat1$age) # 從小到大排的話，原本的index對應到新的index是多少
newdat1$age[ord] # 把age用order排序

orderr<-order(newdat1$age, decreasing=TRUE) # 從大到小排的話，原本的index對應到新的index是多少
newdat1$age[orderr]

plot(newdat1$age[orderr], main="my titleee", xlab="xlab bla", ylab="ylab bla")
abline(h=min(newdat1$age), col="red")# 在最小值畫一條線
abline(h=max(newdat1$age), col="blue")# 在最大值畫一條線

# 預設是一個個點
plot(newdat1$age[orderr], type="h") # p, l, h, s
# 如果要在plot 上加一條新的線，不能再打一次plot(會創造新圖)，而是用point
points(newdat1$age[ord], type="l") #加一條type=l的在原圖上
hist(newdat1$age, br=20) # 共分成幾格

boxplot(newdat1$age[1:100], newdat1$age[101: 300]) # 1~100做一個box, 101~300 做一個box


# heatmap怎用

# t(newdat)是transpose # sd是function求出std
# 標準化！
newdat.std<-(t(newdat)-apply(newdat, 2, mean))/apply(newdat, 2, sd)

install.packages("gplots")
library(gplots) #類似import

# 除此heatmap函数之外，gplots包中的heatmap.2()函数，也可以做热点图聚类。
# heatmap.2(newdat.std, trace="none", col=redgreen)
heatmap(as.matrix(newdat.std))
