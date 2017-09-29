setwd("/Users/adrianhsu/Google_Drive/NTUEE_106_1/sta/0929-lab1")

####################
## Simple algebra ##
####################

3+7
7-3
7*3
7/3
7%%3
round(7/3)
ceiling(7/3)
floor(7/3)
floor(8/3)

############################
## Data import and export ##
############################

dat<-read.table(file="pheno_com.txt", header=TRUE, row.names=1)

class(dat) # data.frame
dat$age # if 一開始有打 header, 則可直接用header名稱獲得此欄資料
dat[,"age"] # 和 matrix不同，取 (i,j)是用中括號
class(dat$age) # numeric -> 此格是存數字
dim(dat) # 330 x 18 matrix
dim(dat$age) 
# 但是dim無法用在向量, e.g. dim(1:5): NULL, 要用 length(1:5) 才會輸出5
length(dat$age)

# columnbind, 也有rbind, 把這三欄提出來比較
newdat<-cbind(dat$age, dat$AD, dat$DDAST)
#View(newdat)
# sep="\t", 即是用tab分割每筆data
# 寫入此檔案
write.table(newdat, file="pheno_com_short.txt", sep="\t", quote=F)

#####################################
## Data structure and manipulation ##
#####################################

## Matrix vs. data frame

class(dat) 
class(newdat) # data.frame
newdat$age 
dat$age # 要轉成data.frame 才能用$ 符號取出vector!

newdat1<-as.data.frame(newdat) #轉成 data.frame的指令
colnames(newdat1)<-c("age", "AD", "DDAST")
rownames(newdat1)<-rownames(dat)
class(newdat1)

# dataframe 可用 $string 去找對應的column name, 就是 map<key, value> 

# Note: newdat1 is a dataframe
class(newdat1)
dim(newdat1)
newdat1
newdat1$age
newdat1[,1]
class(newdat1[,"age"])
class(newdat1$age)
newdat1[1,]
class(newdat1[1,])

# 如何從vec轉成matrix 

n=4
A<-matrix(n, nc=3, nr=3)
A

# Note: newdat1 is a dataframe
class(newdat1)
dim(newdat1)
newdat1
newdat1$age
newdat1[,1]
class(newdat1[,"age"])
class(newdat1$age)
newdat1[1,]
class(newdat1[1,])

## From a vector to a matrix

# nc=4 = # of columns 
A<-1:12
A
B<-matrix(A, nc=4)
B
for (i in 1:3){
	for (j in 1:4){
		print(B[i,j])
	}
}
# 如果填的value, 則submatrix整個都是這值
n = 4
Tmp<-matrix(n, nc=3, nr=3)
則是一個 3*3 , 每個值都是 4。

## Matrix algebra

C<-1:4
B%*%C #矩陣相乘 趴乘以趴
D<-1:3
t(B)%*%D

# 爛
B*C #變成 a11*b1, a12*b2...a14*b4, a21*b1, a22*b2...
B*D

E<-matrix(0.3, nc=3, nr=3)
diag(E)<-1
E
solve(E)
diag(solve(E))

B
upper.tri(B) #哪些地方是tri、哪些不是
B[upper.tri(B)]  # 取出原本B的element值
B[lower.tri(B)]


## Data index and subset

c(1,2,3,4,5)
1:5
c(1,3,5)
seq(1, 5, by=2)

newdat1[1:5, ]
a<-1:5
newdat1[a, ]
b<-c(1,3,5)
newdat1[b, ]

newdat1[b, 1:2]


############################
## Descriptive statistics ##
############################

sum(newdat1$age)
max(newdat1$age)
min(newdat1$age)
median(newdat1$age)
mean(newdat1$age)
sum(newdat1$age)/length(newdat1$age)
var(newdat1$age)
sd(newdat1$age)
sqrt(var(newdat1$age))
summary(newdat1$age)

quantile(newdat1$age)
quantile(newdat1$age, probs=c(0, 1/3, 2/3, 1))
quantile(newdat1$age, probs=seq(0, 1, 0.2))

newdat1
apply(newdat1, 2, mean)
apply(newdat1, 2, median)
apply(newdat1, 1, mean)
apply(newdat1, 2, min)
apply(newdat1, 2, max)
apply(newdat1, 2, quantile)

## reorder the data

newdat1$age[1:3]
newdat1$age[c(2,3,1)]

sort(newdat1$age)
ord<-order(newdat1$age)
newdat1$age[ord]

ord.r<-order(newdat1$age, decreasing=TRUE)
newdat1$age[ord.r]

################
## R graphics ##
################

plot(newdat1$age)
plot(newdat1$age[ord])
plot(newdat1$age[ord.r], main="This is a title", 
	xlab="This is x-axis label", ylab="This is y-axis label")
abline(h=max(newdat1$age))
abline(h=min(newdat1$age))
abline(h=mean(newdat1$age), col="red")
abline(h=median(newdat1$age), col="green")

plot(newdat1$age[ord.r], type="p")
plot(newdat1$age[ord.r], type="l")
plot(newdat1$age[ord.r], type="h")
plot(newdat1$age[ord.r], type="s")

plot(newdat1$age[ord.r], type="l")
points(newdat1$age[ord], type="l")
points(newdat1$age[ord], type="p")

hist(newdat1$age)
hist(newdat1$age, br=20)

boxplot(newdat1$age)
boxplot(newdat1$age[1:100], newdat1$age[101:200])

newdat.std<-(t(newdat)-apply(newdat, 2, mean))/apply(newdat, 2, sd)
install.packages("gplots")
library(gplots)
heatmap.2(newdat.std, trace="none", col=redgreen)


####################################
## Generation of random variables ##
####################################

rv.normal<-rnorm(n=10000, mean=100, sd=15)
hist(rv.normal, br=20)

rv.binomial<-rbinom(n=10000, size=10, prob=0.8)
hist(rv.binomial, br=20, xlim=c(0, 10))

rv.poisson<-rpois(n=10000, lambda=3)
hist(rv.poisson, br=20)

rv.uniform<-runif(n=10000, min=1, max=4)
hist(rv.uniform, br=20)


################
## Resampling ##
################

set.seed(37)

a<-1:5
sample(a, size=5, replace=FALSE)
sample(a, size=5, replace=TRUE)

sample(a, size=3, replace=FALSE)
sample(a, size=3, replace=TRUE)

set.seed(37)
index.norep<-sample(a, size=5, replace=FALSE)
index.rep<-sample(a, size=5, replace=TRUE)
temp<-newdat1$age[1:5]
temp
temp[index.norep]
temp[index.rep]


## Bootstrap

set.seed(37)
rv.normal<-rnorm(n=100, mean=100, sd=15)

mm<-rep(NA, 1000)
for (i in 1:1000){
	set.seed(37+i)
	rv.normal.boot<-sample(rv.normal, replace=TRUE)
	mm[i]<-mean(rv.normal.boot)
}
hist(mm)

# 95% CI from bootstrap
quantile(mm, probs=c(0.025, 0.975))

# 95% CI from CLT
mean(rv.normal)-1.96*sd(rv.normal)/10
mean(rv.normal)+1.96*sd(rv.normal)/10



##############################
## Create your own function ##
##############################

myboot<-function(x, n.boot){

	mm<-rep(NA, n.boot)
	for (i in 1:n.boot){
		set.seed(37+i)
		rv.normal.boot<-sample(x, replace=TRUE)
		mm[i]<-mean(rv.normal.boot)
	}
	hist(mm)
	
	CI.boot<-quantile(mm, probs=c(0.025, 0.975))
	return(CI.boot)

}

set.seed(37)
rv.normal<-rnorm(n=100, mean=100, sd=15)
myboot(rv.normal, n.boot=1000)
myboot(rv.normal, n.boot=5000)


## apply-loop and for-loop

set.seed(37)
rv.normal.long<-rnorm(n=100*50, mean=100, sd=15)
rv.normal.mat<-matrix(rv.normal.long, nr=100)
dim(rv.normal.mat)

# convert a matrix to a list
rv.normal.list<-split(rv.normal.mat, rep(1:ncol(rv.normal.mat), each=nrow(rv.normal.mat)))
class(rv.normal.list)
dim(rv.normal.list)
length(rv.normal.list)
rv.normal.list

# analysis using two different loops
out.forlp<-matrix(NA, nc=50, nr=2)
for (i in 1:50){
	out.forlp[,i]<-myboot(rv.normal.mat[,i], n.boot=1000)
}

out.sapply<-sapply(rv.normal.list, function(x) myboot(x, n.boot=1000))

# two loops produce same results
out.sapply[,1:5]
out.forlp[,1:5]



