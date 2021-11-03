args = commandArgs(trailingOnly = TRUE)

input_file <- args[1]
output_file <- args[2]
epochs <- as.numeric(args[3])
hidden_dim <- as.numeric(args[4])

# Leo y paso a matriz el dataset
dataset <- read.csv(input_file , header = FALSE)
dataset <- as.matrix(dataset)

# Dimensiones de las matrices que se crearán
V_n <- nrow(dataset)
X_n <- ncol(dataset)

# inicialitzacion aleatoria de matrices
X <- matrix(rnorm(hidden_dim * X_n), ncol = hidden_dim)
V <- matrix(rnorm(hidden_dim * V_n), ncol = hidden_dim)

# Para cada una de las iteraciones que lanzamos como argumento 3
for ( epoch in 1: epochs ){
  
  # Para la V
  for ( V_pos in 1:V_n){
    x <- X
    y <- dataset[V_pos, ]
    linear_model <- lm(y~x - 1) # no utilizamos el intercept
    V[V_pos , ] <- linear_model$coefficients
  }
  
  # Recorro la matriz V y paso los negativos a un número muy pequeño
  # Lo hago complejo porque con runif de una vez, asignaba el mismo número
  for (r in 1:nrow(V))
    for (c in 1:ncol(V))
      if (V[r,c]<0)
        V[r,c] <- runif(1,0,0.000001)
  
  # Para la x
  for ( X_pos in 1: X_n){
    x <- V
    y <- dataset[, X_pos ]
    linear_model <- lm(y~x - 1) # no utilizamos el intercept
    X[X_pos, ] <- linear_model$coefficients
  }
  
      
  # Recorro la matriz X y paso los negativos a un número muy pequeño
  # Lo hago complejo porque con runif de una vez, asignaba el mismo número
  for (r in 1:nrow(X))
    for (c in 1:ncol(X))
      if (X[r,c]<0)
        X[r,c] <- runif(1,0,0.000001)
  
  # calculo de la matriz de puntuaciones V * X^T
  scorings <- V %*% t( X )
 
  # Muestro el error de cada iteración, que debe ir acercándose a 0.
  cat (" Error :", mean(( scorings - dataset )^2) , "\n")
  }

# Muestro resultados y creo salida al csv
new_scorings <- ( 1 - dataset ) * scorings + min(scorings) * dataset
results <- data.frame (items = apply(new_scorings, 1, which.max))
print ( results )
write.table ( results , output_file , quote =FALSE , col.names =FALSE , row.names =FALSE )