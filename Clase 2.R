#y=beta0+beta1 x+u

x<- runif(100000,-5,5)
u<- rnorm (100000)
y<- 4+2*x+u
ones<- rep(1,100000)

X<- cbind(ones,x)

#ESTIMACION CON MCO
betas<- solve(t(X)%*%X)%*% t(X)%*% y



#2paso
betas <- function (X,y){
betas <-solve(t(X)%*%X)%*% t(X)%*% y
 return(betas[2])
}


#bucle
for (i in 1:10){print(i)}
for (i in 1:10){print("dani")}
betas1<- c()
for (i in 1:10){
  
s_values<-sample(1:100000, 10)
X_s <- X[s_values,]
y_s <- y[s_values]
#pso 2
beta1<- betas(X_s, y_s)
betas1<- c(betas1,beta1)
}