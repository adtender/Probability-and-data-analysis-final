#1. Read the Final.csv data from D2L to R and Python and denote this data by d1.
d1<-read.csv("C:/Users/WMPCw/Documents/College/Probability/Final/final.csv")

#2. How many observations (number of rows) and Variables (columns) in the d1 data?
dim(d1)

#3. How many variables are numerical/continuous and how many are them are integers/discrete?
str(d1)

#4. Delete ID variable from the d1 data
d1$ID<-NULL

#5. Report the number of missing values for the variables MOFB, YOB, and AOR.
sum(is.na(d1$MOFB))
sum(is.na(d1$YOB))
sum(is.na(d1$AOR))

#6. Create d2 data from d1 data by selecting variables RMOB, WI, RCA, Religion, Region, AOR, HEL, DOBCMC, DOFBCMC, MTFBI, RW, RH, and RBMI variables.
d2<-d1[,c("RMOB", "WI", "RCA", "Religion", "Region", "AOR", "HEL", "DOBCMC", "DOFBCMC", "MTFBI", "RW", "RH", "RBMI")]

#7. Delete rows that have missing values for any variable in the d2 data and denote this new data by d3.
d3<-na.omit(d2)

#8. Find the summary statistics of the d3 data.
summary(d3)

#9. Add a new variable in the d3 data by finding the average of DOBCMC, DOFBCMC and MTFBI.
d3$ABP<-(d3$DOBCMC+d3$DOFBCMC+d3$MTFBI)/3

#10. Create a new variable named “Newreligion” by recoding ‘1’ as ‘1’ and rest as ‘2’ from the Religion Variable.
d3$Newreligion<-ifelse(d3$Religion==1,1,2)

#11. Find the frequency table for the Region variable
table(d3$Region)

#12. Find the joint frequency table for the variables Region and Religion.
table(d3$Region,d3$Religion)

#13. Find the mean values of AOR variable corresponding to each label of Region variable.
aggregate(AOR~Region,data=d3,mean)

#14. Find the variances of AOR variable corresponding to each label of Religion variable.
aggregate(AOR~Religion,data=d3,var)

#15. Draw a boxplot for the MTFBI variable.
boxplot(d3$MTFBI,main="Boxplot for MTFBI variable")

#16. Draw a histogram for the RCA variable.
hist(d3$RCA, main="Histogram for RCA variable")

#17. Draw a bar chart for the Region variable
barplot(d3$Region)

#18. Draw a pie chart for the Region variable
pie(d3$Region)

#19. Put above four figures (question 15 to question 18) in a 2 by 2 grid
par(mfrow=c(2,2))
boxplot(d3$MTFBI,main="Boxplot for MTFBI variable")
hist(d3$RCA, main="Histogram for RCA variable")
barplot(table(d3$Region))
pie(table(d3$Region))

#20. Split the d3 data by WI variable and denote it by d4
d4<-with(d3,split(d3,WI))

#21. For each split data in d4 write a single loop to find the mean, minimum, maximum, standard deviation of MTFBI.
mat11<-matrix(NA,length(d4),5)
for(i in 1:length(d4)){
d<-d4[[i]]
mean1<-round(mean(d$SBP),1)
min1<-round(min(d$SBP),1)
max1<-round(max(d$SBP),1)
var1<-round(var(d$SBP),1)
med1<-round(median(d$SBP),1)
All<-c(mean1,min1,max1,var1,med1)
mat11[i,]<-All
}

#22. Conduct a one sample mean test of hypothesis to check whether MTFBI has a mean of 30 or not.
t.test(d3$MTFBI,mu=30)

#23. Conduct a normality test of the MTFBI variable
install.packages('nortest')
library(nortest)
ad.test(d3$MTFBI)

#24. Check the equality of mean for MTFBI variable corresponding to two labels of “Newreligion” variable.
t.test(d3$MTFBI~d3$Newreligion)

#25. Find the correlation matrix of the variables DOBCMC, DOFBCMC, AOR, MTFBI, RW, RH and RBMI from the d3 data.
corM<-d3[,c(1:7)]
cor(corM)

#26 (blank)


#27. Fit a multiple regression model by considering MTFBI as dependent variable and AOR, RW, Region as independent variables
reg<-lm(MTFBI~AOR+RW,data=d3)
summary(reg)

#28. Simulate one data from the following equation y = 50 + 10x + 20U + 100N + E. 
#Where X is binomial with n=20, p=.70. U is uniform between 15 and 30 (inclusive). 
#N is normal with mean 0 and standard deviation 5. E is random uniform between -1 and 1.
t.mean<-640
t.var<-257920
B=10
n=10
Simdata<-function(n) {
	x=rbinom(n,20,.70)
	u=runif(n,15,30)
	n=round(rnorm(n,0,5),2)
	e=runif(n,-1,1)
	y=50+(10*x)+(20*u)+(100*n)+e
	y=round(y,2)
	y
}
foo<-function(n){
	x<-Simdata(n)
	mean.x<-mean(x)
	var.x<-var(x)
	mv<-c(mean.x,var.x)
	mv
}
foo(n)
a<-replicate(B, foo(n))
sim.mean<-mean(a[1,])
sim.var<-mean(a[2,])
Simdata(n)

#29. Repeat the procedure 100 times and check the true mean with the simulated mean.
foo(100)
abs(t.mean-sim.mean)

#30. Repeat the procedure 100 times and check the true variance with the simulated variance.
abs(t.var-sim.var)

#31. Repeat the procedure 500 times and check the true mean with the simulated mean.
foo(500)
abs(t.mean-sim.mean)

#32. Repeat the procedure 500 times and check the true variance with the simulated variance.
abs(t.var-sim.var)

#33. For five values of x=1:5, y=2:6, and z=3:7, compute 5 values for ??(??) = ((e^x)-log(z^2))/(5+y)
clc<-function(n){
	x=runif(n,1,5)
	y=runif(n,2,6)
	z=runif(n,3,7)
	e <- sum(1/factorial(0:100))
	func=((e^x)-log(z^2))/(5+y)
	func
}
clc(5)


#34. Solve the following system of linear equations: 70x+100y+40z=900; 120x+450y+340z=1000; 230x+230y+1230z=3000
A <- rbind(c(70, 100, 40), 
           c(120, 450, 340), 
           c(230, 230, 1230))
B <- c(900, 1000, 3000)
solve(A, B)


#35. Find the inverse of the following matrix:A=
A <- matrix( c(20, 30, 30,
		   20, 80, 120,
		   40, 90, 360), nrow=3, byrow=TRUE)
install.packages('matlib')
library(matlib)
inv(A)

#36. Suppose b
b <- matrix(c(10, 20, 30),3,1)
AT<-t(A)
temp<-inv(AT%*%A)%*%AT
temp%*%b

#37. Draw the graph for the function f(x)=
e <- sum(1/factorial(0:100))
curve((e^x)/factorial(x), from=2, to=15)

#38.Draw the graph for the step functions Consider the continuous function f(x) = 
e <- sum(1/factorial(0:100))
x <- seq(-5, 15, 0.001)
f <- function(x) {
  if(x<0) {
    fx <- (2*x^2)+(e^x)+3
  } else if (x >= 0 && x < 10) {
    fx <- (9*x)+log(2)
  } else if(x >= 10 ) { 
    fx <- (7*x^2)+(5*x)-17
  }
  fx
}
f <- Vectorize(f)
plot(x, f(x))
x <- seq(-10, 3, 0.01)
plot(x, fx(x), type="l", las=1)


#39. Find the areas of 10 circles, which have radii 10:19. The area of a circle is given pi(r)squared
clc<-function(n){
	x=runif(n,10,19)
	print(x)
	func=pi*x^2
	func
}
clc(10)

#40. Find summation(x=1)(10000)(1/log(x))
x <- 2:10000
sum(1/log10(x))

#41. Compute summation(i=1)(30)summation(j=1)(10)(i to the 10/3+j)
sum((1:30)^10)*sum(1/(3+(1:10)))


#42. Compute the integral Integral(0-inf)(x to the fifteen*e to the negative 40x)dx
integrand <- function(x) {(x^15)*e^(-40*x)}
integrate(integrand, lower = 0, upper = Inf)

#43. Compute the integral Integral(0-1)((x to the 150)(1-x)to the 30)dx
integrand <- function(x) {(x^150)*((1-x)^30)}
integrate(integrand, lower = 0, upper = 1)


#44. For five values of x=1:5, y=2:6, and z=3:7, compute 6 values for f(x) = ((e to the x)-log(z squared))/(5+y)
clc<-function(n){
	x=runif(n,1,5)
	y=runif(n,2,6)
	z=runif(n,3,7)
	e <- sum(1/factorial(0:100))
	func=((e^x)-log(z^2))/(5+y)
	func
}
clc(5)

#45. Solve the equation (x squared) - 33x + 1 = 0
quad <- function(a, b, c){
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}
quad(a = 1, b = -33, c = 1)

#47. If 40 dollars is invested today for 50 years with interest rate .10, the find the total amount of money in 50 years. The formula is p*(1+r) to the t. p=40, t=50, r=.10
p<-40
r<-.10
t<-50
p*(1+r)^t

#48. Fit a simple regression model by using MTFBI as dependent variable and AOR as independent variable
summary(lm(MTFBI~AOR,data=d1))

#49. Check whether AOR and MTFBI are correlated or not.
cor.test(d1$AOR,d1$MTFBI)

#50. Check whether variance of AOR is 10 or not.
install.packages("EnvStats")
library(EnvStats)
varTest(d1$AOR, alternative = "two.sided", conf.level = 0.95, sigma.squared = 10, data.name = NULL)
