
#######################################################################################################
#HEC Lausanne - MScF
#Data Science for Finance
#Exercise Session 2 - 27.09.2019
#Marceau Pierron, Taulant Ukshini, David Sasselli, Nora Koennyu
#######################################################################################################


#######################################################################################################
#Question 2.3
#######################################################################################################


#initialization
parameter = 0.5                             #given parameter lambda
size=c(30,100,1000,10000)                   #sample size
colours=c("black","red","green","blue")     #legend colors

lambda_total=c()

#simulating Poisson random variables and computing their expectations
for(i in size){
	lambda=c()
	for(j in 1:1000){
		x=rpois(i,parameter)
		s=mean(x)
		lambda=c(lambda,s)
	}

	lambda_total=cbind(lambda_total,lambda)

}
#charting the results
par(bty="n")

plot(density(lambda_total[,1]),
    ylim=c(0,60),
    col="black",
    main="Question 2.3 Poisson Distribution",
    xlab="")

for(k in 2:lenght(size)){
    lines(density(lambda_total[,k]),
        col=colours[k])
}

legend(x="right",y=0.92,c("30 Observations","100 Observations","1000 Observations","10000 Observations"),
    cex=0.8,
    lty=1,
    col=colours,
    box.lty=0
)


#######################################################################################################
#Question 2.4
#######################################################################################################

mu_func = function(n){
  lambda=c()
	for(j in 1:1000){
		x=rpois(n,parameter)
		s=mean(x)
		lambda=c(lambda,s)
	}
    return(sd(lambda))
}

func_1 = function(x){
    return(1/sqrt(x))
}

par(bty="n")

plot(func_1,
    main="Question 1.2.2 Poisson Distribution",
    col="red",
    col.main="black",
    lwd=4,
    xlab="Number of Observations",
    ylab="Volatility of Estimator",
    xlim=c(-50,10000),
    ylim=c(0.01,0.2)
)

i=0

while(i <= 10000){
    points(i,mu_func(i),pch=3)
    i=i+200
}

legend(x="right",y=0.92,c("Estimated Volatility of the Estimator","1/sqrt(n)"),
    cex=0.8,
    lty=c(NA,1),
    col=c("black","red"),
    pch=c(NA,3),
    box.lty=0
)

###############################################################################
# Question 2.3
###############################################################################

layout(matrix(1:4,2,2))
for (i in 1:length(size)){
  temp=(lambda_total[,i]-lambda)/(sqrt(parameter))*sqrt(size[i])
  dens=density(temp)
  plot(dens, 
       xlim =c(-4,4),
       ylim =c(0,0.5),
       main=size[i])
  lines(dens$x,dnorm(dens$x),col="red")
  }

