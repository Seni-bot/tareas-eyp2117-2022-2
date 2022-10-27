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
cc <- optim$objective
print(paste("El valor c para f(x) es",round(cc)))

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
  
  simulaciones <- c()
  while(length(simulaciones) < N){
    U1 <- runif(1)
    U2 <- runif(1)
    
    v1 <- 2*U1-1
    v2 <- 2*U2-1
    
    s <- v1^2+v2^2
    
    if(s<=1){
      
      X <- v1*(-2*log(s)/s)^{1/2}
      
      simulaciones <- append(simulaciones, X)
      
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
      see <- vars[i]*(1/2)+(-1)
      comp <- append(comp,see)
    }
    if(U>1/3 & U < 2/3){
      comp <- append(comp, vars[i])
    }
    if(U > 2/3){
      see <- vars[i]*(sqrt(1/2))+(1)
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
  
  while(length(sims) < N){
    U <- runif(1)
    V <- runif(1)
    
    X <- sqrt(-2*log(1-U))*cos(2*pi*V)
    
    sims <- c(sims,X)
  }

  return(sims)
}

composicion <- function(N){
  vars <- box.muller(N)
  comp <- c()
  for(i in 1:N){
    U <- runif(1)
    if(U < 1/3){
      see <- vars[i]*(sqrt(1/4))+(-1)
      comp <- append(comp,see)
    }
    if(U>1/3 & U < 2/3){
      comp <- append(comp, vars[i])
    }
    if(U > 2/3){
      see <- vars[i]*(sqrt(1/2))+(1)
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

# En quarto


# Pregunta 3--------------------------------------------------------
## a)----

par(mfrow = c(1,1))
curve(dnorm(x, mean = 4, sd = sqrt(3)), from = -10, to = 40, lwd = 2, col = "red",
      main = "Normal(4,3) y Gama(shape = 10, rate = 0.7)", ylab = "Densidad")
curve(dgamma(x, shape = 10, rate = 0.7),from = -10, to = 40, lwd = 2, col = "blue",
      add = TRUE)
legend(x = "topright",
       inset = .02,
       lty = c(1,1), 
       col= c("red","blue"), 
       legend=c("Normal", "Gama"),
       cex = 0.8)

## b)----

fx <- function(x){
  fx <- (((0.7)^10)/gamma(10))*x^(10-1)*exp(-0.7*x)
  return(fx)
}

f_g <- function(x){
  f_g <- fx(x)/dweibull(x, shape = 2.5, scale = 13)
  return(f_g)
}

c <- optimise(f_g, maximum = TRUE, interval = c(0,30))$objective


simu.gamma <- function(N){
  simulaciones <- c()
  
  while(length(simulaciones) < N){
    U <- runif(1)
    y <- rweibull(1, shape = 2.5, scale = 13)
    
    if(U <= (1/c)*fx(y)/dweibull(y,shape = 2.5, scale = 13)){
      simulaciones <- append(simulaciones, y)
    }
  }
  return(simulaciones)
}

# Para simular normales usaré el método polar (función polar.method(N))

compos <- function(N, p1){
  simu <- c()
  for(i in 1:N){
    U <- runif(1)
    
    if(U < p1){
      ans <- polar.method(1)
      ans <- (ans * sqrt(3)) + 4 # "Des-estandarizamos" la muestra N(0,1) a N(4,3)
      simu <- append(simu,ans)
    }else{
      simu <- append(simu, simu.gamma(1))
    }
  }
  return(simu)
}

## c) ----
par(mfrow = c(2,2))
N <- 10^5

################# Para p1 = 0 #################
p1_1 <- 0
aux1 <- compos(N, p1_1)
hist(aux1,
     probability = TRUE,
     main = "N = 10^5, p1 = 0",
     breaks = 50,
     ylim = c(0,0.15),
     xlab = "X")

fx <- function(x){
  fx <- dgamma(x, shape = 10, rate = 0.7)
  return(fx)
}

curve(fx, add = TRUE, lwd = 2, col = "red")

##############################################

################# Para p1 = 0.5 ##############

p1_2 <- 0.5
aux2 <-compos(N, p1_2)
hist(aux2,
     probability = TRUE,
     main = "N = 10^5, p1 = 0.5",
     ylim = c(0,0.2),
     breaks = 30,
     xlab = "X")

fx <- function(x){
  fx <- 1/2 * dnorm(x, mean = 4, sd = sqrt(3)) +
    1/2 * dgamma(x, shape = 10, rate = 0.7)
  return(fx)
}

curve(fx, add = TRUE, lwd = 2, col = "red")

##############################################


################# Para p1 = 1 ################

p1_3 <- 1
aux3 <- compos(N, p1_3)
hist(aux3,
     probability = TRUE,
     main = "N = 10^5, p1 = 1",
     breaks = 50,
     ylim = c(0,0.25),
     xlab = "X")

fx <- function(x){
  fx <- dnorm(x, mean = 4, sd = sqrt(3)) 
  return(fx)
}

curve(fx, add = TRUE, lwd = 2, col = "red")

##############################################

## d)----

############### Para p1 = 0 ###################

# valores simulados
simu <- aux1
comparativa_1a <- list(tipo = "simulada",
                       media = mean(simu),
                       desv. = sd(simu),
                       varianza = var(simu))

# valores "reales"

real <- rgamma(N, shape = 10, rate = 0.7)
comparativa_1b <- list(tipo = "real",
                       media = mean(real),
                       desv. = sd(real),
                       varianza = var(real))

df <- data.frame(tipo = c(),
                 media = c(),
                 desv. = c(),
                 varianza = c())

df <- rbind(df, comparativa_1a)
df <- rbind(df, comparativa_1b)
df

###############################################

############### Para p1 = 0.5 #################

# valores simulados
simu <- aux2
comparativa_2a <- list(tipo = "simulada",
                       media = mean(simu),
                       desv. = sd(simu),
                       varianza = var(simu))

# valores "reales"

real <- 0.5 * rnorm(N, mean = 4, sd = sqrt(3)) + 0.5*rgamma(N, shape = 10, rate = 0.7)
comparativa_2b <- list(tipo = "real",
                       media = mean(real),
                       desv. = sd(real),
                       varianza = var(real))

df <- data.frame(tipo = c(),
                 media = c(),
                 desv. = c(),
                 varianza = c())

df <- rbind(df, comparativa_2a)
df <- rbind(df, comparativa_2b)
df

###############################################

############### Para p1 = 1 ###################

# valores simulados
simu <- aux3
comparativa_3a <- list(tipo = "simulada",
                       media = mean(simu),
                       desv. = sd(simu),
                       varianza = var(simu))

# valores "reales"

real <- rnorm(N, mean = 4, sd = sqrt(3))
comparativa_3b <- list(tipo = "real",
                       media = mean(real),
                       desv. = sd(real),
                       varianza = var(real))

df <- data.frame(tipo = c(),
                 media = c(),
                 desv. = c(),
                 varianza = c())

df <- rbind(df, comparativa_3a)
df <- rbind(df, comparativa_3b)
df

###############################################