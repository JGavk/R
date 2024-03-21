iq_personas <- c(119, 109, 124, 119, 106, 112, 112, 112,
                 112, 109, 112, 124, 109, 109, 109, 106, 124, 112, 112,106)
  iq_ordenados <- sort(unique(iq_personas))
#   1. Para este analisis lo clasificaremos como Datos cuantitativos discreto, debido a que los datos son facilmente 
  # acomodados en repeticiones, no hay mucha variacion y es un grupo muy cerrado de datos, asi facilitaremos su analisis

  tablaIQ <- as.data.frame(table(IQ=iq_personas))
#Frecuencia acumulada, frecuencia relativa, frecuencia relativa acumulada
  tablaIQ <- transform(tablaIQ,
          FreAcu = cumsum(Freq),
          FreRel = round(prop.table(Freq),2),
          RelAc = round(cumsum(prop.table(Freq)),2))
#Obtengo la frecuencia acumulada de la transformacion
  freq_acumulada <- cumsum(tablaIQ$Freq)
#hist(iq_personas,main = "Datos de IQ",xlab= "IQ tomado", ylab = "Frecuencia", xlim=c(105,126))
  barplot(tablaIQ$Freq, 
          names.arg = tablaIQ$IQ,
          main = "Datos de IQ", 
          xlab = "IQ", 
          ylab = "Frecuencia")
  #Crear la Ojiva de los datos ordenados anterior mente por iq_ordenados tomando la frecuencia acumulada de la tablaIQ
  plot(iq_ordenados, freq_acumulada, type = "o", 
       main = "Ojiva de IQ", xlab = "IQ", ylab = "Frecuencia acumulada")
  #Tala para la moda, mediana y media
  tabla_moda <- table(iq_personas)
  #Calculando la moda ordenando las frecuencias de mayor a menor con el parametro decreasing para ordenar descendente
  moda <- names(sort(tabla_moda, decreasing = TRUE))[1]
  mediana <- median(iq_personas)
  media <- mean(iq_personas)
  desviacion_media <- mean(abs(iq_personas - mediana))
  var_range <- diff(range(iq_personas))
  desviacion_estandar <- round(sd(iq_personas),2)
  #Agregamos los resultados en la tabla resultados
  resultados_mid <- data.frame(
    Moda = moda,
    Mediana = mediana,
    Media = media,
    DesMed = desviacion_media,
    DesEs = desviacion_estandar,
    Rango_var = var_range
    
  )
  resultados_mid
  
  
  battery_life <- c(2.2, 3.4, 2.5, 3.3, 4.7,
                    4.1, 1.6, 4.3, 3.1, 3.8,
                    3.5, 3.1, 3.4, 3.7, 3.2,
                    4.5, 3.3, 3.6, 4.4, 2.6,
                    3.2, 3.8, 2.9, 3.2, 3.9,
                    3.7, 3.1, 3.3, 4.1, 3.0,
                    3.0, 4.7, 3.9, 1.9, 4.2,
                    2.6, 3.7, 3.1, 3.4, 3.5)
  #Obtenemos el tamaño del arreglo
  n <- length(battery_life)
  
  #Obtenemos el intervalo por la formula con log
  class_quantity <- 1 + 3.3 * log10(n)
  # calculo de intervalos
  data_range <- diff(range(battery_life))
  anc_intervalo <- data_range/class_quantity
  intervalos <- cut(battery_life, breaks = seq(min(battery_life),
                                               max(battery_life) + anc_intervalo, by = anc_intervalo), right = FALSE)
  frecuencias <- table(intervalos)
  
  
  # Calcula las frecuencias
  frecuencia_relativa <- prop.table(frecuencia_absoluta)
  frecuencia_acumulada <- cumsum(frecuencia_absoluta)
  frecuencia_relativa_acumulada <- cumsum(frecuencia_relativa)
  tabla_distribucion <- data.frame(
    Intervalo = names(frecuencias),
    Frecuencia = as.vector(frecuencias),
    Frec_Relativa = as.vector(frecuencia_relativa),
    Frec_Acu = as.vector(frecuencia_acumulada),
    FrecRel_Acu = as.vector(frecuencia_relativa_acumulada)
  )
  # Dibujar el histograma y obtener los datos
  hist_data <- hist(battery_life, plot = FALSE)
  
  # Obtener los upper points de cada intervalo
  upper_points <- hist_data$breaks[-1]
  
  # Se intenta poner los puntos en la linea superior de cada barra del histograma
  ajustar_puntos <- (hist_data$breaks[-1] + hist_data$breaks[-length(hist_data$breaks)]) / 2
  
  # Utilizar densidades para el polígono, asegurando que se ajuste a las barras del histograma
  densities <- hist_data$density
  
  # Dibujar el histograma
  hist(battery_life, plot = TRUE, freq = FALSE,
       main = "Histograma de Duración de la Batería con Polígono",
       xlab = "Duración de la Batería",
       ylab = "Densidad de Frecuencia")
  
  # Dibujar el polígono de frecuencia
  lines(ajustar_puntos, densities, type = "b", col = "blue", lwd = 2, pch = 19)
  #Toma los puntos medios de los intervalos y los lleva a la frecuencia en que se dan
  plot(midpoints, frecuencia_acumulada, type = "o", 
       main = "Ojiva de Batería", xlab = "Duración de la Batería", ylab = "Frecuencia Acumulada", col= "green")
  