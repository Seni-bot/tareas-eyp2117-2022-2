library(ggplot2)
library(plotly)
library(pracma)


set.seed(2117)
# Pregunta 1 ----
## a) ----

integral <- function(x,y){
  hx <-  x*y^2*sqrt(x^2+y^3)
  return(hx)
}

rango_x <- seq(0,3,length = 50)
rango_y <- seq(0,2,length = 50)
z <- outer(rango_x,rango_y,integral)

par(bg = "gray70")
persp(rango_x, rango_y, z,
      main = "Volumen a integrar",
      xlab = "x",
      ylab = "y",
      theta = 35, phi = 15,
      col = "cyan"
      )

## c)----

estimador <- function(N){
  U1 <- runif(N,0,3)
  U2 <- runif(N,0,2)
  hx <- U1*U2^2*sqrt(U1^2+U2^3)*6
  return(mean(hx)) 
}

## d) ----
N_1 <- 10^2
N_2 <- 10^3
N_3 <- 10^4
#Para n = 10^2
estimador(N_1)
#para n = 10^3
estimador(N_2)
#Para n = 10^4
estimador(N_3)
#Valor real
valor_real <- pracma::integral2(integral, 0, 3, 0, 2)
valor_real[1]


#Pregunta 2 ----
## b) ----
sim <- function(N){
  U <- runif(N)
  g <- log(U/(1-U))
  
  hx <- (1/sqrt(2*pi)*exp(-g^2/2)*(exp(-(g-3)^2/2)+
         exp(-(g-6)^2/2)))*1/(U*(1-U))
  
  return(mean(hx))
}

## c)----

N <- 10^4
seq <- seq(1,N)
muestras <- c()
for(i in 1:N){
  muestras <- append(muestras, sim(i))
}
par(bg = "gray70")
plot(seq, muestras, xlab = "Realizaciones", ylab = "Valor estimado",
     main = "Estimación de E[h(x)]", col = "deepskyblue2")

## d)----

funcion_esp <- function(x){
  hx <- (1/sqrt(2*pi)*exp(-x^2/2)*(exp(-(x-3)^2/2)+exp(-(x-6)^2/2)))
  return(hx)
}

valor_real_esp <- pracma::integral(funcion_esp,-Inf,Inf)
valor_real_esp

N_1 <- 10^4
seq_1 <- seq(1,N_1)
precision <- c()
for(i in 1:N_1){
  precision <- append(precision, valor_real_esp - sim(i))
}
par(bg = "gray70")
plot(seq_1, precision, xlab = "Realizaciones", ylab = "Precisión",
     main = "Precisión de la estimación de E[h(x)]", col = "navy")

# Pregunta 3 ----
## d) ----
funcion_ppartes <- function(a,b,N){
  U <- runif(N)
  vector_respuesta <- NULL
  for(i in 1:N){
    if(U[i]<1/2){
      hx <- 2*a+(b-a)*sqrt(2*U[i])
      vector_respuesta <- c(vector_respuesta,hx)
    }else{
      hx <- 2*b+(a-b)*sqrt(2*(1-U[i]))
      vector_respuesta <- c(vector_respuesta,hx)
    }
  }
  return(vector_respuesta)
}

## e) ----

N = 10^4
a = 0
b = 5

hx_1 <- function(x){
  hx <- x/25
  return(hx)
  }

hx_2 <- function(x){
  hx <- (10-x)/25
  return(hx)
}

aux <- funcion_ppartes(a,b,N)
par(bg = "gray70")
hist(aux, prob = T, main= "Histograma f(x)", col = "forestgreen", xlab = "X")
curve(hx_1, from=0, to=5, add = TRUE, col = "red", lwd = 3)
curve(hx_2, from=5, to=10, add = TRUE, col = "red", lwd = 3)
