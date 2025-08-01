
# Función principal
generar_tabla_y_grafico <- function(data, var_fila, var_columna) {
  
  # Crear tabla de contingencia
  
  tabla <- table(data[[var_fila]],data[[var_columna]], useNA = "no")
  
  ######################################  # cambiar tipo de estructura 
  tabla <- as.data.frame.matrix(tabla)
  tabla <- na.omit(tabla)
  
  tabla[[var_fila]] <- rownames(tabla)
  
  print(tabla)
  #return(tabla)
  ###################################### # cambiar formato de:  wide data frame --> long data frame 
  #### cambiar a long data frame 
  
  tabla_long <- tabla %>%
    pivot_longer(
      cols = -all_of(var_fila),
      names_to = "Resultado",
      values_to = "Frecuencia"
    )
  
  ########################################### # graficar 
  # Graficar con ggplot2
  barras <- ggplot(tabla_long, aes(x = !!sym(var_fila), y = Frecuencia, fill = Resultado)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = var_fila, y = "Frecuencia", fill = "Resultado") +
    theme_minimal()
  
  print(barras)
  
}