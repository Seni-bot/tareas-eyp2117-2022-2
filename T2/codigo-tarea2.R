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
