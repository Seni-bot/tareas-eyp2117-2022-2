# Código tarea 2 - EYP2117
# Maximiliano Medina

# Pregunta 1 ------------------------------------------------------
## a)----

fx <- function(x){
  fx <- (x+(2*sin(x)))/(exp(x))
  return(fx)
}

curve(fx, from = 0, to = 2, ylim = c(0,1.5),
      main = "Función f(x)",
      ylab = "f(x)", xlab = "Valores",
      lwd = 2)
## b) ----
# Proponemos una densidad gamma con shape = 2

f_g <- function(x){
  f_g <- fx(x)/dgamma(x, shape = 2)
  return(f_g)
}

optim <- optimise(f_g, maximum = TRUE, interval = c(0,2))
(cc <- optim$objective) 


## c)----
curve(fx, from = 0, to = 10, lwd = 2, ylim = c(0,1.5), ylab = "" )

curve(dgamma(x,shape = 2), from = 0, to = 10,lwd = 2,
      add =TRUE, col = "red")

curve(cc*dgamma(x,shape = 2), from = 0, to = 10,lwd = 2,
      add =TRUE, col = "green")


## d)----
acep_rech <- function(n){
  aceptados <- 0
  rechazados <- 0
  simulaciones <- c()
  
  while(length(simulaciones) < n){
    y <- rgamma(1, shape = 2)
    u <- runif(1)
    
    if(u <=(1/cc)*fx(y)/dgamma(y, shape = 2)){
      aceptados <- aceptados + 1
      simulaciones <- append(simulaciones, y)
    }else{
      rechazados <- rechazados +1
    }
  }
  tasa_acep <- aceptados/(aceptados + rechazados)
  
  const_norm <- 1/(cc*tasa_acep)
  
  return(list(simulaciones,tasa_acep,const_norm))
}


## e)----

par(mfrow = c(2,2))

N <- 100
test_1 <- acep_rech(N)
hist(test_1[[1]], probability = TRUE, breaks = 40, ylim = c(0,1),
     ylab = "Densidad", xlab = "Simulaciones",
     main = "N = 100")
curve(fx(x)*test_1[[3]],lwd = 3,
      add =TRUE, col = "blue")

N <- 1000
test_2 <- acep_rech(N)
hist(test_2[[1]], probability = TRUE, breaks = 40, ylim = c(0,1),
     ylab = "Densidad", xlab = "Simulaciones",
     main = "N = 1000")
curve(fx(x)*test_2[[3]],lwd = 3,
      add =TRUE, col = "red")

N <- 10000
test_3 <- acep_rech(N)
hist(test_3[[1]], probability = TRUE, breaks = 40, ylim = c(0,1),
     ylab = "Densidad", xlab = "Simulaciones",
     main = "N = 10000")
curve(fx(x)*test_3[[3]],lwd = 3,
      add =TRUE, col = "green")

N <- 20000
test_4 <- acep_rech(N)
hist(test_4[[1]], probability = TRUE, breaks = 45, ylim = c(0,1),
     ylab = "Densidad", xlab = "Simulaciones",
     main = "N = 20000")
curve(fx(x)*test_4[[3]],lwd = 3,
      add =TRUE, col = "darkmagenta")

# Pregunta 2 ------------------------------------------------------
## a) ----

mezcla <- function(x){
  mez <- 1/3*dnorm(x, mean = -1, sd = 1/2) +
    1/3*dnorm(x, mean = 0, sd = 1) +
    1/3*dnorm(x, mean = 1, sd = sqrt(1/2))
  return(mez)
}

par(mfrow =c(1,1))

curve(mezcla(x), from = -10, to = 10, lwd = 2, col = "red", ylim = c(0,1.8))

curve(dnorm(x, mean = -1, sd = 1/2), lwd = 2,lty = 2, add = TRUE, col = "blue")

curve(dnorm(x, mean = 0, sd = 1), lwd = 2, lty = 2, add = TRUE, col = "blue")

curve(dnorm(x, mean = 1, sd = sqrt(1/2)), lwd = 2,lty = 2, add = TRUE, col = "blue")

## b)----

polar.method <- function(N){
  #Notar que el método polar solo genera para N par
  simulaciones <- c()
  while(length(simulaciones) < N){
    U1 <- runif(1)
    U2 <- runif(1)
    
    v1 <- 2*U1-1
    v2 <- 2*U2-1
    
    s <- v1^2+v2^2
    
    if(s<=1){
      X <- v1*(-2*log(s)/s)^{1/2}
      Y <- v2*(-2*log(s)/s)^{1/2}
      
      simulaciones <- append(simulaciones, c(X,Y))
      
    }
  }
  return(simulaciones)
}

composicion <- function(N){
  vars <- polar.method(N)
  comp <- c()
  for(i in 1:N){
    U <- runif(1)
    if(U < 1/3){
      see <- vars[i]*(1/2)-(-1)
      comp <- append(comp,see)
    }
    if(U>1/3 & U < 2/3){
      comp <- append(comp, vars[i])
    }
    if(U > 2/3){
      see <- vars[i]*(sqrt(1/2))-(1)
      comp <- append(comp, see)
    }
  }
  return(comp)
}

## c)----

par(mfrow = c(2,2))

N <- 100
hist(composicion(N), probability = TRUE, main ="N = 100",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")

N <- 1000
hist(composicion(N), probability = TRUE, main ="N = 1000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")


N <- 10000
hist(composicion(N), probability = TRUE, main ="N = 10000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")


N <- 20000
hist(composicion(N), probability = TRUE, main ="N = 20000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")


## d)----

box.muller <- function(N){
  sims <- c()
  for(i in 1:N){
    U <- runif(1)
    V <- runif(1)
    
    X <- sqrt(-2*log(1-U))*cos(2*pi*V)
    Y <- sqrt(-2*log(1-U))*sin(2*pi*V)
    
    sims <- c(sims,c(X,Y))
  }
  return(sims)
}

composicion <- function(N){
  vars <- box.muller(N)
  comp <- c()
  for(i in 1:N){
    U <- runif(1)
    if(U < 1/3){
      see <- vars[i]*(1/2)-(-1)
      comp <- append(comp,see)
    }
    if(U>1/3 & U < 2/3){
      comp <- append(comp, vars[i])
    }
    if(U > 2/3){
      see <- vars[i]*(sqrt(1/2))-(1)
      comp <- append(comp, see)
    }
  }
  return(comp)
}

## e)----

par(mfrow = c(2,2))

N <- 100
hist(composicion(N), probability = TRUE, main ="N = 100",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")

N <- 1000
hist(composicion(N), probability = TRUE, main ="N = 1000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")


N <- 10000
hist(composicion(N), probability = TRUE, main ="N = 10000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")


N <- 20000
hist(composicion(N), probability = TRUE, main ="N = 20000",
     xlab = "x", ylab = "Densidad", breaks = 40, ylim = c(0,1))
curve(mezcla, from = -10, to = 10, add = T, col = "red")



## f)----

#En quarto


# Pregunta 3--------------------------------------------------------
## a)----

par(mfrow = c(1,2))
curve(dnorm(x, mean = 4, sd = 3), from = -10, to = 20, lwd = 2, col = "red")
curve(dgamma(x, shape = 10, rate = 0.7), from = 0, to = 40, lwd = 2, col = "blue")

## b)----
fx <- function(x){
  fx <- x^(10-1)*exp(-0.7*x)
  return(fx)
}


f_g <- function(x){ 
  f_g <- dgamma(x,shape = 10,rate = 0.7)/dweibull(x,shape = 2.5, scale = 13)
  return(f_g)
}

oo <- optimise(function(x){dgamma(x,shape = 10,rate = 0.7)/dweibull(x,shape = 2.5, scale = 13)}, maximum = TRUE, lower = 0,upper = 30)

optim <- optimize(f_g, maximum = TRUE, lower = 0, upper = 100) 
cc <- optim$objective

### ESPERAR ANUNCIO DE PROFE, PROBABLEMENTE ESTE MALO EL ENUNCIADO, MIENTRAS TRABAJAR CON C(0,30) COMO INTERVALO

simu_gamma <- function(N){
  aceptados <- 0
  rechazados <- 0
  simulaciones <- c()
  
  while(length(simulaciones) < N){
    y <- rweibull(1,shape = 2.5, scale = 13)
    u <- runif(1)
    
    if(u <=(1/cc)*fx(y)/dweibull(y,shape = 2.5, scale = 13)){
      aceptados <- aceptados + 1
      simulaciones <- append(simulaciones, y)
    }else{
      rechazados <- rechazados + 1
    }
  }
  tasa_aceptacion <- aceptados/(aceptados+rechazados)
  return(list(simulaciones, tasa_aceptacion))
}

simu_gamma(10)


## c)----
par(mfrow=c(2,2))
N <- 100

p1 <- 0
caso_1 <- func_mez(N,p1)
hist(caso_1)# ver bien el dominio
curve()# ACA VA LA GAMMA MULTIPLICADA POR M, QUE TENGO QUE CALCULARLO XLAXUXA

p1 <- 0.5
caso_2 <- func_mez(N,p1)

p1 <- 1
caso_3 <- func_mez(N,p1)

## d)----




