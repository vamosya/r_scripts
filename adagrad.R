args = commandArgs ( trailingOnly = TRUE )

input_file <- args[1]
output_file <- args[2]
epochs <- as.numeric(args[3])
hidden_dim <- as.numeric (args[4])
lambda <- as.numeric(args[5])

## Añado el una pequeña constante delta para asegurar que el denominador no es 0.
delta <- as.numeric(args[6])

dataset <- read.csv(input_file , header = FALSE )
dataset <- as.matrix(dataset)

users_n <- nrow (dataset)
items_n <- ncol (dataset)

# inicialitzacion aleatoria de matrices
items_descriptions <- matrix(runif(hidden_dim * items_n ) , ncol = hidden_dim )
users_descriptions <- matrix(runif(hidden_dim * users_n) , ncol = hidden_dim )

# inicializo el gradiente acumulado r
r <- 0

for ( epoch in 1: epochs ) {
  
  ## predicciones dadas por la mutliplicacion de dos matrices
  predictions <- users_descriptions %*% t(items_descriptions)
  
  ## valores actualizados de la matriz descriptiva de los items
  for ( item_pos in 1: items_n) {
    prediction_error <- dataset[, item_pos , drop = FALSE ] - predictions [, item_pos , drop = FALSE ]
    for (k in 1: hidden_dim ){
      ## Lo que tengo que modificar es el incremento en cada iteracion
      g <- 2/ epoch * (t(users_descriptions[, k , drop = FALSE ]) %*% prediction_error )
      g <- as.numeric(g)
      
      #Aplico el gradiente acumulado
      r <- r + g^2
      
      adagrad_uptate <- ((-1 * lambda) /(delta+ sqrt(r))) * g
        
      items_descriptions[ item_pos , k] <- items_descriptions[item_pos , k ] - adagrad_uptate
    }
  }
  
  ## valores actualizados de la matriz descriptiva de usuarios
  for ( user_pos in 1:users_n) {
    prediction_error <- dataset[ user_pos , , drop = FALSE ] - predictions[ user_pos , , drop = FALSE ]
    for (k in 1:hidden_dim ){
      g <- 2 / epoch * ( prediction_error %*% items_descriptions[, k , drop = FALSE ])
      g <- as.numeric( g )
      
      #Aplico el gradiente acumulado
      r <- r + g^2
      
      adagrad_uptate <- ((-1 * lambda) /(delta+ sqrt(r))) * g
      
      users_descriptions[ user_pos , k] <- users_descriptions[ user_pos , k ] - adagrad_uptate
    }
  }
  
  # calculo de la matriz de puntuaciones usuario - item
  scorings <- users_descriptions %*% t( items_descriptions )
  cat(" Error :", mean(( scorings - dataset )^2) , "\n")
}


print( scorings )

# esta linia pone los items ya consumidos a valor minimo
new_scorings <- (1 - dataset ) * scorings + min ( scorings ) * dataset
results <- data.frame( items = apply( new_scorings , 1 , which.max) )

print( results )

write.table( results , output_file , quote = FALSE , col.names = FALSE , row.names = FALSE )