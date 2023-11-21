#1
x<-c(0,1,2,3,4)
p<-c(0.41,0.37,0.16,0.05,0.01)

sum(x*p)

#2

f<-function(t)
{
  t*0.1*exp(-0.1*t)
}

integrate(f,lower=0,upper=Inf)



#3

x_val<-c(0,1,2,3)
pmf_x<-c(0.1,0.2,0.2,0.5)

#12x+2(3-x)-18

e_x<-sum(x_val*pmf_x)

y=10*e_x-12

y


#4

f1<-function(x)
{
  x*0.5*exp(-x)
}

f2<-function(x)
{
  x*x*0.5*exp(-x)
}

mean<-integrate(f1,1,10)$value
second_m<-integrate(f2,1,10)$value

cat("Mean is ",mean)
cat("Variance is ", second_m-mean^2)




