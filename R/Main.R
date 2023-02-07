
# Define pro-rating function here
Prorate <- function(C,x_m)
{
  Ones <- rep( 1,length(x_m) )
  
  x_n <- as.vector(C %*% diag(x_m) %*% Ones)
  
  x_n <- diag(1/x_n)
  
  x_n[x_n == Inf] <- 0
  
  M <- x_n %*% C %*% diag(x_m)
  
  # For more information on  pro-rating see SI (page 23 or section S4.2) of Lenzen 2012 
  # (Link: https://doi.org/10.1021/es300171x)
  
  return(M)
}

# Create a n by m binary concordance matrix where n < m
name_n <- c("Sheep_products", "Cattle_products", "Wheat_and_rye", "Products_nec")
name_m <- c("Meat", "Milk", "Wool", "Wheat", "Other_grains")   
C <- matrix(data = c(1,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,0,0,1,1), nrow = 4, ncol = 5 )

# Define proxy for x_m:
x_m <- c(100,20, 10, 20,25)

rowSums(Prorate(C, x_m))
Prorate(C, x_m)
