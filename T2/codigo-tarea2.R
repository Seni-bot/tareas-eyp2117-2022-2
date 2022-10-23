# Código tarea 2 - EYP2117
# Maximiliano Medina

# Pregunta 1 ----
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
(cc <- optim$objective) #PREGUNTAR SI SE SACA C ASÍ O EN PAPEL


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

# Pregunta 2 ----
## a) ----

mezcla <- function(x){
  mez <- 1/3*dnorm(x, mean = -1, sd = 1/4) +
    1/3*dnorm(x, mean = 0, sd = 1) +
    1/3*dnorm(x, mean = 1, sd = 1/2)
  return(mez)
}

par(mfrow =c(1,1))
curve(mezcla(x), from = -10, to = 10, lwd = 2, col = "red", ylim = c(0,1.8))

curve(dnorm(x, mean = -1, sd = 1/4), lwd = 2,lty = 2, add = TRUE, col = "blue")

curve(dnorm(x, mean = 0, sd = 1), lwd = 2, add = TRUE, col = "blue")

curve(dnorm(x, mean = 1, sd = 1/2), lwd = 2,lty = 2, add = TRUE, col = "blue")

## b)----



############### ESTA MALA ESTA WEA #######################
### PREGUNTAR SI DEBO USAR EL METODO DE LA CLASE / BUSCAR LA PDF DE LA MULTIPLICAICON DE LAS 3 NORMALES
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

## c)----

par(mfrow = c(2,2))

N <- 100
hist(polar.method(N), probability = TRUE, main ="N = 100",
     xlab = "x", ylab = "Densidad", breaks = 40)


N <- 1000
hist(polar.method(N), probability = TRUE, main ="N = 1000",
     xlab = "x", ylab = "Densidad", breaks = 40)


N <- 10000
hist(polar.method(N), probability = TRUE, main ="N = 10000",
     xlab = "x", ylab = "Densidad", breaks = 40)


N <- 20000
hist(polar.method(N), probability = TRUE, main ="N = 20000",
     xlab = "x", ylab = "Densidad", breaks = 40)


###########################################################


  # Pregunta 3----
## a)----

par(mfrow = c(1,2))
curve(dnorm(x, mean = 4, sd = 3), from = -10, to = 20, lwd = 2, col = "red")
curve(dgamma(x, shape = 10, rate = 0.7), from = 0, to = 40, lwd = 2, col = "blue")

## b)----

func_mez <- function(N, p1){
  
  fx <- function(x,a,b){
    fx <- b^a/gamma(a)*x^(a-1)*exp(-b*x)
    return(fx)
  }
  
  f_g <- function(x){ #ESTO DE ACA ME DA UNA FUNCION MONOTONA CRECIENTE
    f_g <- fx(x,10,0.7)/dweibull(x,shape = 2.5, scale = 13)
    return(f_g)
  }
  
  optim <- optimise(f_g, maximum = TRUE, interval = c(-10,50)) #DEFINIR BIEN EL INTERVALO
  cc <- optim$objective
  
  acep_recha <- function(N){
    simulaciones <- c()
    
    while(length(simulaciones) < N){
      y <- rweibull(1,shape = 2.5, scale = 13)
      u <- runif(1)
      
      if(u <=(1/cc)*fx(y,10,0.7)/dweibull(y,shape = 2.5, scale = 13)){
        simulaciones <- append(simulaciones, y)
      }
    }
    return(simulaciones)
  }#SUPONIENDO QUE ME SALIÓ EL SIMULAR LAS GAMMA
  
  sims <- acep_recha(N)
  
  fx_x <- p1*rnorm(N, mean = 4, sd = 3) + (1-p1)*sims
  return(fx_x)
}

## c)----
par(mfrow=c(2,2))
N <- 10^5

p1 <- 0
caso_1 <- func_mez(N,p1)
hist(caso_1)# ver bien el dominio
curve()# ACA VA LA GAMMA MULTIPLICADA POR M, QUE TENGO QUE CALCULARLO XLAXUXA

p1 <- 0.5
caso_2 <- func_mez(N,p1)

p1 <- 1
caso_3 <- func_mez(N,p1)

## d)----


