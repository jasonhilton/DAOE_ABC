set.seed(1434)
test_function <- function(X, B){
  cbind(1, X, X[,1] * X[,2], X[,1]*X[,1]*X[,1]*X[,1], 
        sin(X[,2]*4*pi)) %*% as.matrix(B)
}

# define the true function so that it ignores any additional columns of X
get_true_function <- function(true_beta){
  true_func <- function(X){
    test_function(X[,1:2], true_beta)
  }
  return(true_func)
}

# some arbitrary coefficients
true_beta <- c(3,-0.5,0.25, 2, -1/10,1)
true_function  <- get_true_function(true_beta)


k <- 2
N <- 100
x <- seq(0,1, 0.05)

# generate a simple grid like design for ease of plotting
X <- as.matrix(expand.grid(replicate(k, x, simplify = F)))
# Observations
Y <- true_function(X)

# stick this in a single dataset
X_df <- as_tibble(X) %>% mutate(y=Y[,1])

saveRDS(X_df, file="example_data/test_function.rds")


