algoritmoRyA <- function(FO, TI, CR, DIR){
  
  # Lanzo la solución por Simplex del modelo
  node1 <- solveLP(FO, TI, CR, maximum=TRUE, const.dir=DIR, lpSolve = TRUE)
  
  # Guardamos la solución de variables y el valor de la función
  node1_sol <- unname(node1$solution)
  node1_opt <- node1$opt
  
  cat("PASOS DEL ÁRBOL\n")
  
  cat("NODO 1:\n")
  
  cat ("La solución de variables es",node1_sol,
       "\n y el valor de la función", node1_opt, "\n\n")
  
  # Numero de soluciones para luego iterar
  num_sol <- length(node1$solution)
  
  # Itero para ir viendo si las soluciones son enteras o no
  # Si no lo es alguna, lanza FALSE y sale, para provocar que se ramifique
  for (i in 1:num_sol){
    
    if (is.integer(node1$solution[i]) == TRUE){
      
      entero_nodo1 <- TRUE
      
    } else {
      
      num_variable <- i
      cat("     Variable para acotar:",num_variable,"\n")
      
      #Establezco el entero por debajo y por arriba de la variable no entera
      bajo <- floor(node1$solution[i])
      alto <- ceiling(node1$solution[i])
      entero_nodo1 <- FALSE
      break
      
    }
    
  }
  
  cat ("     Acoto en ", bajo, "y", alto, "\n\n")
  
  
  if (entero_nodo1 == FALSE){
    
    cat("     Resuelvo el nodo 2:")
    
    # Creo el vector de coeficiente de la restricción nueva
    # Con la posición del 1 para la variable de la restricción
    CRv_2 <- rep(0, length(FO))
    CRv_2[num_variable] <- 1
    
    # Paso los términos independientes para añadir la cota baja
    TI_2 <- TI
    TI_2[length(TI)+1] <- bajo
    
    # Paso el vector sin nombre fila
    CR_2 <- rbind(CR,CRv_2,deparse.level = FALSE)
    
    # Añado la desigualdad de la nueva restricción
    DIR_2 <- DIR
    DIR_2[length(DIR)+1] <- "<="
    
    # Igual que en el nodo 1
    node2 <- solveLP(FO, TI_2, CR_2, maximum=TRUE, const.dir=DIR_2, lpSolve = TRUE)
    node2_sol <- unname(node2$solution)
    node2_opt <- node2$opt
    cat ("     La solución de variables es",node2_sol,
         "\n y el valor de la función", node2_opt, "\n\n")
    
    for (i in 1:num_sol){
      
      if (is.integer(node2$solution[i] == TRUE)){
        
        entero_nodo2 <- TRUE
        
      } else {
        
        entero_nodo2 <- FALSE
        break
        
      }
      
    }
    
    
    # Igual que en el nodo 2, pero en lugar de trabajar con la cota baja,
    # La restricción es con la cota alta.
    
    cat("     Resuelvo el nodo 3:")
    CRv_3 <- rep(0, length(FO))
    CRv_3[num_variable] <- 1
    TI_3 <- TI
    TI_3[length(TI)+1] <- alto
    CR_3 <- rbind(CR,CRv_3,deparse.level = FALSE)
    DIR_3 <- DIR
    DIR_3[length(DIR)+1] <- ">="
    
    node3 <- solveLP(FO, TI_3, CR_3, maximum=TRUE, const.dir=DIR_3, lpSolve = TRUE)
    
    node3_sol <- unname(node3$solution)
    node3_opt <- node3$opt
    cat ("     La solución de variables es",node3_sol,
         "\n y el valor de la función", node3_opt, "\n\n")
    
    for (i in 1:num_sol){
      
      if (node3$solution[1] %% 1 == 0){
        
        entero_nodo3 <- TRUE
        
      } else {
        
        entero_nodo3 <- FALSE
        break
        
      }
      
    }
    
    # Condiciones para mostrar si es entera o no
    
    if (entero_nodo1 == TRUE || entero_nodo2 == TRUE || entero_nodo3 == TRUE){
      cat("TENEMOS UNA SOLUCIÓN ENTERA!!!\n")
    } else {
      cat("No hay ninguna solucón entera :-( \n Elegimos la de valor mayor...\n")
    }
    
    # Condiciones para elegir la solución óptima
    # Si hay solución entera, la consideramos óptima
    # Si no hay solución entera, elegimos la de mayor valor
    
    if ( entero_nodo2 == TRUE && entero_nodo3 == TRUE ) {
      
      if (node2_opt > node3_opt){
        
        cat("CONCLUSION:")
        cat ("La solución óptima está en el nodo 2 con punto óptimo",node2_sol,
             "\n y el valor de la función", node2_opt, "\n\n")
        
      } else if (node3_opt > node2_opt) {
        
        cat("CONCLUSION:")
        cat ("La solución óptima está en el nodo 3 con punto óptimo",node3_sol,
             "\n y el valor de la función", node3_opt, "\n\n")
        
      }
      
    } else if (entero_nodo2 == TRUE && entero_nodo3 == FALSE) {
      
      cat("CONCLUSION:")
      cat ("La solución óptima está en el nodo 2 con punto óptimo",node2_sol,
           "\n y el valor de la función", node2_opt, "\n\n")
      
    } else if (entero_nodo2 == FALSE && entero_nodo3 == TRUE){
      
      cat("CONCLUSION:")
      cat ("La solución óptima está en el nodo 3 con punto óptimo",node3_sol,
           "\n y el valor de la función", node3_opt, "\n\n")  
      
    } else if (entero_nodo2 == FALSE && entero_nodo3 == FALSE){
      
      if (node2_opt > node3_opt && node2_opt > node1_opt) {
        
        cat("CONCLUSION:")
        cat ("La solución óptima está en el nodo 2 con punto óptimo",node2_sol,
             "\n y el valor de la función", node2_opt, "\n\n")
        
      } else if (node3_opt > node1_opt && node3_opt > node1_opt){
        
        cat("CONCLUSION:")
        cat ("La solución óptima está en el nodo 3 con punto óptimo",node3_sol,
             "\n y el valor de la función", node3_opt, "\n\n")
        
      } else if (node1_opt > node3_opt && node1_opt > node2_opt){
        
        cat("CONCLUSION:")
        cat ("La solución óptima está en el nodo 1 con punto óptimo",node1_sol,
             "\n y el valor de la función", node1_opt, "\n\n")
        
      }
      
    }
    
  }   
  
}