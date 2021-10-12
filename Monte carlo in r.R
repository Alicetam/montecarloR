#Q1
data_set <- read.csv('/Users/Alice/Desktop/GM_and_Ford.csv', header = TRUE)

GM_price <- data_set$GM_Price
set.seed(11111)
p <- 100 
t <- 400
n <- length(GM_price)
GM_rt <- log(GM_price[-1]/GM_price[-n])

GM_matrix <- matrix(nrow = p, ncol = t)

for (i in 1:p){
  for (j in 1:t){
    GM_mean <- mean(GM_rt)*j
    GM_sd <- sd(GM_rt)*sqrt(j)
    r=rnorm(1,GM_mean,GM_sd)
    GM_matrix[i,j] <- GM_price[n]*exp(r)
  }
  
}

matplot(t(GM_matrix),type = 'l',xlab = 'Period',ylab = 'GM Price',
        main='Monte Carlo Simulation for GM Price')


Mean_Trend <- c()
for (i in 1:t) {
  m <- mean(GM_matrix[,i])
  Mean_Trend <- c(Mean_Trend,m)
}
plot(Mean_Trend,xlab = 'Period',ylab = 'Mean Price',main='Trend of Mean')

sd_Trend <- c()
for (i in 1:t) {
  sd <- sd(GM_matrix[,i])
  sd_Trend <- c(sd_Trend,sd)
}
plot(sd_Trend,xlab = 'Period',ylab = 'Volatility',main='Trend Volatility')


#Q2
set.seed(22222)
p <- 2000 
t <- 200
n <- length(GM_price)
GM_rt <- log(GM_price[-1]/GM_price[-n])

GM_matrix <- matrix(nrow = p, ncol = t)

for (i in 1:p){
  for (j in 1:t){
    GM_mean <- mean(GM_rt)*j
    GM_sd <- sd(GM_rt)*sqrt(j)
    r=rnorm(1,GM_mean,GM_sd)
    GM_matrix[i,j] <- GM_price[n]*exp(r)
  }
  
}
mean(GM_matrix[,100])
mean(GM_matrix[,200])
sd(GM_matrix[,100])
sd(GM_matrix[,200])

#Q3
count <- ifelse(GM_matrix[,200]>GM_price[n],1,0)
probability <- sum(count)/p
print(probability)


#Q4 Strategy 1

p <- 2000
t <- 6
n <- length(GM_price)
GM_rt <- log(GM_price[-1]/GM_price[-n])
GM_matrix <- matrix(nrow = p, ncol = t+1)
GM_matrix[,1] <- rep(GM_price[n])
p_return <- c()

for (i in 1:p){
  for (j in 2:t){
    GM_mean <- mean(GM_rt)*(j-1)
    GM_sd <- sd(GM_rt)*sqrt(j-1)
    r=rnorm(1,GM_mean,GM_sd)
    GM_matrix[i,j] <- GM_price[n]*exp(r)
    if (GM_matrix[i,j] < GM_matrix[i,j-1]){
      path_return=GM_matrix[i,j]/GM_price[n]
      p_return <- c(p_return,path_return)
      break
    } 
  }
}

expected_return <- mean(p_return)  

matplot(t(GM_matrix),type = 'l',xlab = 'Period',ylab = 'GM Price',
        main='Strategy 1')

sprintf('The Expected return of strategy 1 is %s',expected_return)



set.seed(33333)
p <- 2000 
t <- 100
n <- length(GM_price)
GM_rt <- log(GM_price[-1]/GM_price[-n])
GM_matrix <- matrix(nrow = p, ncol = t+1)
GM_matrix[,1] <- rep(GM_price[n])
p_return <- c()



for (i in 1:p){
  GM_mean_2001 <- mean(GM_rt)*(1) 
  GM_sd_2001 <- sd(GM_rt)*sqrt(1)
  r_2001=rnorm(1,GM_mean_2001,GM_sd_2001)
  P_2001 <- GM_price[n]*exp(r_2001)
  GM_matrix[i,2] <- P_2001
}


for (i in 1:p){
  for (j in 3:t){
    GM_mean <- mean(GM_rt)*(j-1)
    GM_sd <- sd(GM_rt)*sqrt(j-1)
    r=rnorm(1,GM_mean,GM_sd)
    GM_matrix[i,j] <- GM_price[n]*exp(r)
    if (GM_matrix[i,j] < GM_matrix[i,j-1]){
      if (GM_matrix[i,j-1]<GM_matrix[i,j-2]){
        path_return=GM_matrix[i,j]/GM_price[n]
        p_return <- c(p_return,path_return)
        break
      }
    } 
  }
}

expected_return <- mean(p_return) 

matplot(t(GM_matrix),type = 'l',xlab = 'Period',ylab = 'GM Price',
        main='Strategy 2')

sprintf('The Expected return of strategy 2 is %s',expected_return)