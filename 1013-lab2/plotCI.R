plotCI <- function(mean, li, ui){
  n <- length(li)
  index <- c(1:n)
  plot(1,mean,type = 's',ylim = range(li,ui,mean),xlim = range(index),
       xaxt = 'n',yaxt = 'n',
       xlab = 'index',ylab = 'mean')
  axis(1,at=seq(1,n,1))
  axis(2,at=seq(floor(min(li,ui)),floor(max(li,ui)),by=1))
  abline(h=mean,col = 'red')
  segments(index,li,index,ui)
}