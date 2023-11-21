#1
 pop<- c(rep("gold",20),rep("silver",30),rep("bronze",50))
 
 samples<- sample(pop,10,replace = FALSE)
 
 cat(samples)
 
 
 #2
 outcomes<-c("Success","Failure")
 probability<-c(0.9,0.1)
 
 sampleS<- sample(outcomes,10,replace = TRUE,probability)
 
 print(sampleS)
 
 
 
 #4
 
 calc <- function(pc,pr,pcr){
   prc<-((pcr*pr)/pc)
   return (prc)
 }
 
 pc=0.4
 pr=0.2
 pcr=0.85
 
 ans<- calc(pc,pr,pcr)
 
 cat(ans)
 
 #5
 
 data(iris)
 
 head(iris)
 
 str(iris)
 
 range(iris$Sepal.Length)
 
 mean(iris$Sepal.Length)
 
 median(iris$Sepal.Length)
 
 q1<-quantile(iris$Sepal.Length,0.25)
 q2<-quantile(iris$Sepal.Length,0.75)
 
 iqr=q2-q1
 
 print(iqr)
 
 cat("Standard Deviation is ", sd(iris$Sepal.Length))
 cat("Variance is ", var(iris$Sepal.Length))
 
 summary(iris)
 
 #6
 
 data <- c(1, 2, 2, 3, 4, 4, 4, 5)
 
 # table(data)
 
 calc_mod<-function(x){
   val=table(x)
   mod_val=max(val)
   res=as.integer(names(val[val==mod_val]))
   return(res)
 }
 
 print(calc_mod(data))
