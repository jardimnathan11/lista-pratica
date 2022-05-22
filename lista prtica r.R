library(ggplot2)
library(xtable)
library(stargazer)
#questao 2
############

# Definindo objetos dos parâmetros
mux=5 #média x
sigma2x=1 #variância x

# Parâmetros
b0 = 1.5
b1 = 3
BETA = c(b0 , b1)
mu1=-2
mu2=2
sigma1=0.5
sigma2=25
a=3
b=6
p= 0.5
#Gerando a amostra
set.seed(1)
x = rnorm(n = 500 ,mean = mux,sd = sqrt(sigma2x))

##gerandp erros

n = 500
U =runif(n)# gerando uma uniforme

#variavel para estocar as amostras da mistura                                            
e1 = rep(NA,n)

#Sgerando as amostras da mistura


for(i in 1:n){
  if(U[i]< p){
    e1[i] = rnorm( 1 ,mu1, sqrt(sigma1))
  }else{
    e1[i] = rnorm( 1 ,mu2, sqrt(sigma2))
  }
}


e2 = rgamma(n = 500, shape = a, rate = b )

set.seed(5)
e3 = rcauchy(n = 500, location = 0, scale = 1)

# gerando amostras dos ys, dado x
y1 = b0 + b1*x + e1
modelo1 = cbind(y1, x, e1)
colnames(modelo1) = c('explicada', 'explicativa', 'erro')

y2 = b0 + b1*x + e2
modelo2 = cbind(y2, x, e2)
colnames(modelo2) = c('explicada', 'explicativa', 'erro')

y3 = b0 + b1*x + e3
modelo3 = cbind(y3, x, e3 )
colnames(modelo3) = c('explicada', 'explicativa', 'erro')

#analise grafica
hist(x , col=mycol1, # column color
     border="black",
     prob = TRUE,  # show densities instead of frequencies
     xlab= " Amostra X",
     xlim=c(0,10), ylim = c(0, 0.5))

lines(density(x), col='red')

#modelo 1
 hist(modelo1[,1] , col=mycol1, # column color
       border="black",
       prob = TRUE,  # show densities instead of frequencies
       xlab= " Amostra 1",
       main = " Modelo 1",xlim=c(-10,40), ylim = c(0, 0.25))

lines(density(modelo1[,1]), col='red')
hist(modelo1[,3],prob = TRUE,
     col=mycol2, add=T)
lines(density(modelo1[,3]), col='red')
legend(x= "topright", legend=c("Y1 amostral", "Erro amostral 1 ", "fitted line"), 
       fill = c(mycol1,mycol2, "red") )
#modelo 2
hist(modelo2[,1] , col=mycol1, # column color
           border="black",
           prob = TRUE,  # show densities instead of frequencies
           xlab= " Amostra 2",
           main = " Modelo 2",xlim=c(-5,25), ylim = c(0, 1.8))
 
lines(density(modelo2[,1]), col='red')
 hist(modelo2[,3],prob = TRUE,
             col=mycol2, add=T)
 lines(density(modelo2[,3]), col='red')
 legend(x= "topright", legend=c("Y2 amostral", "Erro amostral 2 ", "fitted line"), 
                fill = c(mycol1,mycol2, "red") )
 
###modelo 3

 hist(modelo3[,1] , col=mycol1, # column color
      border="black",
      prob = TRUE,  # show densities instead of frequencies
      xlab= " Amostra 3",
      main = " Modelo 3",xlim=c(-20,50), ylim = c(0, 0.3))
 
 lines(density(modelo3[,1]), col='red')
 hist(modelo3[,3],prob = TRUE,
      col=mycol2, add=T)
 lines(density(modelo3[,3]), col='red')
 legend(x= "topright", legend=c("Y3 amostral", "Erro amostral 3 ", "fitted line"), 
        fill = c(mycol1,mycol2, "red") )
### resultados tabela

media= c(mean(x),mean(e1), mean(y1), mean(e2),mean(y2), mean(e3), mean(y3) )
mediana= c(median(x),median(e1), median(y1), median(e2),median(y2), median(e3), median(y3) )
variancia = c(var(x), var(e1), var(y1), var(e2),var(y2), var(e3), var(y3) )
minimo = c(min(x),min(e1), min(y1), min(e2),min(y2), min(e3), min(y3) )
maximo = c(max(x) ,max(e1), max(y1), max(e2),max(y2), max(e3), max(y3) )

estats = data.frame(cbind(media, mediana, variancia, minimo, maximo))
rownames(estats) = c('x' ,'e1 ', 'y1 ', 'e2 ', 'y2 ', 'e3 ', 'y3 ' )
xtable(estats, type = "latex", file = "estats.tex")
########################

#questao 3
#########
reg1 = lm( y1 ~ x)
summary(reg1)
coefficients(reg1)
reg2 = lm( y2 ~ x)
summary(reg2)
coefficients(reg2)
reg3 = lm( y3 ~ x)
summary(reg3)
coefficients(reg3)
coeficientes=rbind.data.frame(coefficients(reg1),coefficients(reg2),coefficients(reg3))
row.names(coeficientes)=c('modelo 1','modelo 2','modelo 3')
colnames(coeficientes)=c('beta 0', 'beta 1')
xtable(coeficientes, type = "latex", file = "coef.tex")
# fazer as tabelas agora
# no c fazer o teste t e argumentar q funciona pelo tcl com as hipoteses ditas em sala com 1 e 2, mas nao com 3

###########

# questao 4
N=500


BTS<- function(x){
  w<- matrix(0, 500, 2000)
  for(j in 1:2000){
    
    z<-numeric(500)
    
    for(i in 1:500){
      
      #Resample 
      k<- sample(x, length(x), replace= TRUE)
      z[i] <- mean(k)
      
    }
    w[,j]<-z
  }
  return(w)
  
  
}
set.seed(0)
BTSX= BTS(x)
set.seed(0)
BTSY1 <-BTS(y1)
set.seed(0)
BTSY2= BTS(y2)
set.seed(0)
BTSY3= BTS(y3)
BTSY1

BTS_coef<-function(a,b){
  BTSM=matrix(0,2000,2)
  for(i in 1:2000){
    reg=lm(a[,i] ~ b[,i] )
    BTSM[i,]<-coefficients(reg)
    
  }
  colnames(BTSM)<-c('b0','b1')
  return(BTSM)
}

BTSM1<-data.frame(BTS_coef(BTSY1, BTSX))
BTSM2<-data.frame(BTS_coef(BTSY2, BTSX))
BTSM3<-data.frame(BTS_coef(BTSY3, BTSX))
ggplot(BTSM1, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
 ggtitle( 'B1 modelo BTS 1  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(BTSM1, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle( 'B0 modelo BTS 1  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(BTSM2, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle( 'B1 modelo BTS 2  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(BTSM2, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle( 'B0 modelo BTS 2  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(BTSM3, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle( 'B1 modelo BTS 3  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(BTSM3, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle( 'B0 modelo BTS 3  MQO') +
  theme(plot.title = element_text(hjust = 0.5))

####################

#questao 5

log_like_normal <- function(theta){
  
  N <- length(x)
  b0 <- theta[1]
  b1 <- theta[2]
  mu1<-theta[3]
  sigma1 <- theta[4] 
  mu2<-theta[5]
  sigma2 <- theta[6] 
  
  loglike  = sum(log((1/(2*(sqrt(2*pi*sigma1)))*exp((-1/2*sigma1)*(y1 - b0 -b1*x - mu1)^2) 
                      + (1/(2*(sqrt(2*pi*sigma2)))*exp((-1/2*sigma2)*(y1 - b0 -b1*x - mu2)^2))))) 
  
  return(-loglike)
}

MLE_estimates <- optim(fn=log_like_normal,            
                       par=c(1.5, 3, -2, 1, 2, 25))
# lower = c(-Inf, -Inf,-Inf,0.0001,-Inf ,0.0001),         
# upper = c(Inf, Inf,Inf,Inf,Inf,Inf),          
#hessian=TRUE,                  # retornar varioancia
#method = "L-BFGS-B")


MLE_par_n <- MLE_estimates$par
MLE_par_n<-round(MLE_par_n, 2)

MLE_par_n <-rbind(c('beta0', 'beta1', 'mu1', 'sigma1', 'mu2', 'sigma2'), MLE_par_n)

xtable(MLE_par_n, type = "latex", file = "MLE_norm.tex")

###
#b
log_like_gamma <- function(theta){
  
  N <- length(x)
  beta0 <- theta[1]
  beta1 <- theta[2]
  a <- theta[3]
  b <- theta[4]
  e       <- y2 - beta0*as.matrix(rep(1,N)) -beta1*x 
  loglike  = sum(log(pgamma(e, shape = a , rate = b )))
  return(-loglike)
}

MLE_estimates <- optim(fn=log_like_gamma,            
                       par=c(0,3,3,6),                  #partida  
                       lower = c(-Inf, -Inf,0.000001,0.0000001),         
                       upper = c(Inf, Inf,Inf, Inf),          
                       hessian=TRUE,                  # retornar varioancia
                       method = "L-BFGS-B")


MLE_par_g <- MLE_estimates$par
MLE_par_g<-round(MLE_par_g, 2)
MLE_par_g <-rbind(c('beta0', 'beta1', 'a', 'b'),MLE_par_g)

MLE_par_g
xtable(MLE_par_g, title('modelo 2' ), type = "latex", file = "MLE_g.tex")
MLE_sd_g <- sqrt(diag(solve(MLE_estimates$hessian)))
MLE_sd_g<-round(MLE_sd_g, 2)
MLE_sd_g <-rbind(c('beta0', 'beta1', 'a', 'b'),MLE_sd_g)

MLE_sd_g
xtable(MLE_sd_g, type = "latex", file = "MLE_sd_g.tex")

###########
#c

log_like_t <- function(theta){
  
  n <- length(x)
  beta0 <- theta[1]
  beta1 <- theta[2]
  r <- theta[3]
  e       <- y3 - beta0*as.matrix(rep(1,N)) -beta1*x 
  loglike  = n*log(gamma((r+1)/2)) -n*log(sqrt(r*pi)*gamma(r/2)) -((r+1)/2)*sum(log(1 +(e^2)/2))
  return(-loglike)
}

MLE_estimates <- optim(fn=log_like_t,            
                       par=c(1,1,1),                  #partida  
                       lower = c(-Inf, -Inf,1),         
                       upper = c(Inf, Inf,Inf),          
                       hessian=TRUE,                  # retornar varioancia
                       method = "L-BFGS-B")


MLE_par_t <- MLE_estimates$par
MLE_par_t<-round(MLE_par_t, 2)
MLE_par_t<-rbind(c('beta0', 'beta1', 'r'),MLE_par_t)
MLE_par_t
xtable(MLE_par_t,title('Modelo 3'), type = "latex", file = "MLE_t.tex")
MLE_sd_t <- sqrt(diag(solve(MLE_estimates$hessian)))
names(MLE_sd_t )<-c('beta0', 'beta1', 'r')
xtable(MLE_sd_t, type = "latex", file = "MLE_sd_t.tex")
##########
# questao 6
#1
MLEBTS_par_n = matrix(0, ncol = 6, nrow= 2000)
MLEBTS_sd_n = matrix(0, ncol = 3, nrow= 2000)
for (i in 1:2000) {
  log_like_n_BTS <- function(theta){
    j<-i
    N <- length(x)
    b0 <- theta[1]
    b1 <- theta[2]
    mu1<-theta[3]
    sigma1 <- theta[4] 
    mu2<-theta[5]
    sigma2 <- theta[6] 
    
    loglike  = sum(log((0.5/(sqrt(2*pi*sigma1)))*exp((-1/2*sigma1)*(BTSY1[,j] - b0 -b1*BTSX[,j] - mu1)^2) 
                       + (0.5/(sqrt(2*pi*sigma2)))*exp((-1/2*sigma2)*(BTSY1[,j] - b0 -b1*BTSX[,j]  - mu2)^2))) 
    
    return(-loglike)
  }
  MLE_estimates <- optim(fn=log_like_n_BTS,            
                         par=c(0, 3, 0, 1, 1, 25))                  #partida  
  #lower = c(-Inf, -Inf,-Inf,0, 0 ,0),         
  # upper = c(Inf, Inf,Inf, Inf,Inf, Inf),          
  #hessian=TRUE,                  # retornar varioancia
  #method = "L-BFGS-B")
  MLEBTS_par_n[i,] <- MLE_estimates$par
  
  
}

MLEBTS_par_n<-data.frame(MLEBTS_par_n)
colnames(MLEBTS_par_n)<-c('b0','b1','mu1','sigma1', 'mu2','sigma2')

ggplot(MLEBTS_par_n, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
  
ggplot(MLEBTS_par_n, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


########## modelo 2

MLEBTS_par_g = matrix(0, ncol = 4, nrow= 2000)
MLEBTS_sd_g = matrix(0, ncol = 3, nrow= 2000)

for (i in 1:2000) {
  log_like_gamma <- function(theta){
    j<-i
    N <- length(x)
    beta0 <- theta[1]
    beta1 <- theta[2]
    a <- theta[3]
    b <- theta[4]
    e       <- BTSY2[,j] - beta0*as.matrix(rep(1,N)) -beta1*BTSX[,j]
    loglike  = sum(log(pgamma(e, shape = a , rate = b )))
    return(-loglike)
  }
  
  MLE_estimates <- optim(fn=log_like_gamma,            
                         par=c(0,3,3,6),                  #partida  
                         lower = c(-Inf, -Inf,0.000001,0.0000001),         
                         upper = c(Inf, Inf,Inf, Inf),          
                         hessian=TRUE,                  # retornar varioancia
                         method = "L-BFGS-B")
  
  
  MLEBTS_par_g[i,] <- MLE_estimates$par
}
MLEBTS_par_g<-data.frame(MLEBTS_par_g)
colnames(MLEBTS_par_g)<-c('b0','b1','a','b')
ggplot(MLEBTS_par_g, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(MLEBTS_par_g, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

########### modelo 3
MLEBTS_par_t = matrix(0, ncol = 3, nrow= 2000)
MLEBTS_sd_t = matrix(0, ncol = 3, nrow= 2000)

for (i in 1:2000) {
  log_like_t <- function(theta){
    
    j<-i
    n <- length(x)
    beta0 <- theta[1]
    beta1 <- theta[2]
    r <- theta[3]
    e       <- BTSY3[,j] - beta0*as.matrix(rep(1,N)) -beta1*BTSX[,j] 
    loglike  = n*log(gamma((r+1)/2)) -n*log(sqrt(r*pi)*gamma(r/2)) -((r+1)/2)*sum(log(1 +((e)^2)/2))
    return(-loglike)
  }
  
  
  
  MLE_estimates <- optim(fn=log_like_t,            
                         par=c(1,3,1))                  #partida  
  #lower = c(-Inf, -Inf,1),         
  #upper = c(Inf, Inf,Inf),          
  #hessian=TRUE,                  # retornar varioancia
  #method = "L-BFGS-B")
  
  
  MLEBTS_par_t[i,] <- MLE_estimates$par
  
  
}

MLEBTS_par_t<-data.frame(MLEBTS_par_t)
colnames(MLEBTS_par_t)<-c('b0','b1','r')

ggplot(MLEBTS_par_t, aes(x=b1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(MLEBTS_par_t, aes(x=b0)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
