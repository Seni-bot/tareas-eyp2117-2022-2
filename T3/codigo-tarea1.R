### Tarea 3 ###

# 1)----



# 2)----

## a)----

fx_mc <- function(x){
  aux <- x/(1+exp(x))*5
  return(aux)
}

U <- runif(10^5,5,10)
sims_mc <- fx_mc(U)
theta_mc <- mean(sims_mc)

## b)----

fx <- function(x){
  aux <- x/(1+exp(x))
  return(aux)
}

theta_int <- integrate(fx,lower = 5, upper = 10)$value

## c)----

#Control 1
control_1 <- function(x){
  fx <- exp(-1/2)/(1+x^2)
  return(fx)
}

U <- runif(10^5,5,10)
X <- fx_mc(U)
Y <- control_1(U)

c_est_1 <- function(x,y){
  c <- -cov(x,y)/var(y)
  return(c)
}

c_estrella_1 <- c_est_1(X,Y)

sims_control_1 <- X + c_estrella_1*(Y - mean(Y))

theta_control_1 <- mean(sims_control_1)

#Control 2

control_2 <- function(x){
  aux <- x+1
  return(aux)
}

U <- runif(10^5,5,10)
X <- fx_mc(U)
Y <- control_2(U)

c_est_2 <- function(x,y){
  c <- -cov(x,y)/var(y)
  return(c)
}

c_estrella_2 <- c_est_2(X,Y)

sims_control_2 <- X + c_estrella_2*(Y - mean(Y))

theta_control_2 <- mean(sims_control_2)

## d)----

#MC
err_1 <- 100*abs(theta_mc-theta_int)/theta_int

#Control1

err_2 <- 100*abs(theta_control_1-theta_int)/theta_int

#Control2
err_3 <- 100*abs(theta_control_2-theta_int)/theta_int

## e)----

var_mc <- var(sims_mc)
var_c1 <- var(sims_control_1)
var_c2 <- var(sims_control_2)

red_c1 <- 100*abs(var_c1 - var_mc)/var_mc
red_c2 <- 100*abs(var_c2 - var_mc)/var_mc

## f)----


par(mfrow = c(1,2))
plot(sims_mc, type = "l", col = "hotpink2", lwd = 3,
     xlab = "Cantidad estimaciones", ylab = "Valores theta",
     main = "Monte carlo vs Var control 1",
     ylim = c(0,0.23))
lines(sims_control_1, type = "l", lwd = 2, col = "darkviolet")
legend("topright", legend = c("Monte carlo","Var de control 1"),
       col = c("hotpink3", "darkviolet"), lwd = 3:2, lty = 1:1, cex = 0.7)

plot(sims_mc, type = "l", col = "cyan4", lwd = 3,
     xlab = "Cantidad estimaciones", ylab = "Valores theta",
     main = "Monte carlo vs Var control 2",
     ylim = c(0,0.23))
lines(sims_control_2, type = "l", lwd = 2, col = "blue3")
legend("topright", legend = c("Monte carlo","Var de control 2"),
       col = c("cyan4", "blue3"), lwd = 3:2, lty = 1:1, cex = 0.7)


# 3)----

cx <- function(D,x){
  aux <- (2*pi)^{-D/2}*exp(-1/2*t(x)%*%x)
  return(aux)
}

U <- runif(1,-5,5)







