### First point ###
#Set the random seed, sample size and true parameters (betas)
set.seed(90000)
n=200
b0=0.5
b1=0.3
#Draw n observations from normal distributions 
x=rnorm(n,0,1)
u=rnorm(n,0,1)
#Specify the linear model 
y=b0+b1*x+u

### Second point ###
#Get OLS estimations for betas
lm.M1=lm(y~x)
summary(lm.M1)

### Third point ###
#Summary method for calculating variance of b1_hat
varM1_b1hat=(0.06882)^2
varM1_b1hat
#Alternative way for calculating variance of b1_hat via Standard Error of the Regression (SER)
SER=sd(resid(lm.M1)*sqrt((n-1)/(n-2)))
seb1hat=SER/(sd(x)*sqrt(n-1))
(seb1hat)^2

### Fourth point ###
#90% confidence interval for betas
confint(lm.M1, level=0.90)

### Fifth point ###
#a) Since xi and ui are independent for assumption of the problem, we can interpret that E[ui|xi]=0, then E[b1_hat]=b1 
#b) Since ui are i.i.d. and xi and ui are independent for assumption of the problem, we can interpret that every ui has the same variance and then var[ui|xi]=var[ui]=1 (conditionally homoskedastic)
#c) Since strong assumption holds (E[ui|xi]=0) for this model, correlation implies causality and then b1_hat estimates the causal effect of x on y

### Sixth point ###
#Run a regression in which independent and dependent variables are switched
lm.M2=lm(x~y)
#Get estimations for gammas
summary(lm.M2)

### Seventh point ###
#Since y, the regressor of the new model, is correlated with new error component, gamma1_hat is not estimating causal effect of y on x neither asymptotically
#Compute gamma1_hat for 10000 times (n going to infinity)
gamma1_hat=numeric(10000)
for (j in 1:10000) 
  {
gamma_hat=coef(lm(x~y))
gamma1_hat[j]=gamma_hat["y"]
}
#Get the mean of all 10000 gamma1_hat to obtain the limit in probability
gamma1_plim=mean(gamma1_hat[j])
gamma1_plim

### Eighth point ###
#Since csi_hat=1/b1_hat represents a non-linear transformation and b1_hat is consistent for b1, csi_hat can approximate to a linear function by Taylor expansion in a neighborhood of b1 
#Moreover, since the first derivative of the transformation exists and non-zero, Delta method is applicable to derive csi_hat distribution
#Given that b1_hat density distribution is asymptotically normal, the distribution of csi_hat will be asymptotically normal as well
library (alr3)
vcov=vcov(lm.M1)
beta_hat=coef(lm(y~x))
b1_hat=beta_hat["x"]
deltaMethod(lm.M1,"1/b1_hat", vcov.=vcov)


