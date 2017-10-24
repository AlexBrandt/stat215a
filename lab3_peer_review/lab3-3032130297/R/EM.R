# Give the true Parameters
pi <- 0.3
mu0 <- 5
mu1 <- 10
n <- 5000

# Generate data
data = NULL
z = NULL
for (i in 1:n){
  if (sample(c(0, 1), 1, prob = c(pi, 1-pi))){
    data = c(data, rpois(1, mu1))
    z = c(z, 1)
  } else {
    data = c(data, rpois(1, mu0))
    z = c(z, 0)
  }
}

# Plot data
hist(data)

# initialize the start points
pi_t <- 0.5
mu0_t <- sample(data, 1)
mu1_t <- sample(data, 1)

# Function for calculate Tji
Tji <- function(j, i, data){
  pi_p0 <- (pi_t * mu0_t^data[i] * exp(-mu0_t)) / (factorial(data[i]))
  pi_p1 <- ((1-pi_t) * mu1_t^data[i] * exp(-mu1_t)) / (factorial(data[i]))
  if (j == 0){
    Tji <- pi_p0 / (pi_p0 + pi_p1)
  } else {
    Tji <- pi_p1 / (pi_p0 + pi_p1)
  }
  return(Tji)
}

# Loop for update the parameters
# initilize the stop criteria
stop_loop <- FALSE

while (!stop_loop) {
  s_t0 <- 0
  s_t1 <- 0
  s_t0x <- 0
  s_t1x <- 0
  for (i in 1:n){
    s_t0 = s_t0 + Tji(0, i, data)
    s_t1 = s_t1 + Tji(1, i, data)
    s_t0x = s_t0x + Tji(0, i, data) * data[i]
    s_t1x = s_t1x + Tji(1, i, data) * data[i]
  }
  stop_loop <- (abs(pi_t - s_t0 / n) < 0.00001) & (abs(mu0_t - s_t0x / s_t0) < 0.00001) & (abs(mu1_t - s_t1x / s_t1) < 0.00001)
  pi_t = s_t0 / n
  mu0_t = s_t0x / s_t0
  mu1_t = s_t1x / s_t1
}

# Get the clustering results based on the estimated parameters
z_t = NULL
for (i in 1:n){
  if (Tji(1, i, data)>0.5){
    z_t = c(z_t, 1)
  } else {
    z_t = c(z_t, 0)
  }
}

# Calculate the accuracy for parameter estimation and clustering.
if (mu0_t < mu1_t){
  acc = c((pi_t - pi) / pi, (mu0_t - mu0) / mu0, (mu1_t - mu1) / mu1, mean(z == z_t))
} else {
  acc = c(((1 - pi_t) - pi) / pi, (mu1_t - mu0) / mu0, (mu0_t - mu1) / mu1, 1 - mean(z == z_t))
}

acc
