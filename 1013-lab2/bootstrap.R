rv.normal<-rnorm(n=10000, mean=100, sd=15)
hist(rv.normal, br=20)

rv.binomial<-rbinom(n=10000, size=10, prob=0.8)
hist(rv.binomial, br=20, xlim=c(0,15))

rv.poisson<-rpois(n=10000, lambda=3)
hist(rv.poisson, br=20)

rv.uniform<-runif(n=10000, min=1, max=4)
hist(rv.uniform, br=20)

set.seed(37)

a<-1:5

#  這種做法是 resampling
sample(a, size=5, replace=TRUE) # 抽取之後放回
sample(a, size=3, replace=FALSE)

# bootstrap
set.seed(37) # seed 改了才會影響sample抽樣的順序
rv.normal<-rnorm(n=100, mean=100, sd=15)
mm<-rep(NA, 1000) #rep replicates the values in x. I
for (i in 1:1000){
  set.seed(37+i)
  rv.normal.boot<-sample(rv.normal, replace=TRUE)
  mm[i]<-mean(rv.normal.boot)
} 

quantile(mm, probs=c(0.025, 0.975))

# RESULT: 
# > quantile(mm, probs=c(0.025, 0.975))
# 2.5%     97.5% 
#   94.90946 100.03599 

mean(rv.normal)-1.96*sd(rv.normal)/10
mean(rv.normal)+1.96*sd(rv.normal)/10

# function 寫在哪都可以
myboot<-function(x, n.boot) { #不用宣告x, n.boot是啥型態
  mm<-rep(NA, n.boot)
  for (i in 1:n.boot){
    set.seed(37+i)
    rv.normal.boot<-sample(x, replace=TRUE)
    mm[i]<-mean(rv.normal.boot)
  } hist(mm)
  CI.boot<-quantile(mm, probs=c(0.025, 0.975))
  return(CI.boot)
}

set.seed(37)
rv.normal<-rnorm(n=100, mean=100, sd=15)
myboot(rv.normal, n.boot=1000)
myboot(rv.normal, n.boot=5000)


