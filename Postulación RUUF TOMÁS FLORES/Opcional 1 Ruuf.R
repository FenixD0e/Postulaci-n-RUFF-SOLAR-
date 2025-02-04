#Verificar si un punto (x, y) está dentro del triángulo isósceles
dentro_del_triangulo <- function(x, y, base, altura) {
#Límites de la pendiente de los dos lados del triángulo
  y_limite_izquierdo <- (2 * altura / base) * x
  y_limite_derecho <- (-2 * altura / base) * x + 2 * altura
  return(y >= 0 & y <= y_limite_izquierdo & y <= y_limite_derecho)
}
#Verificar si un panel está completamente dentro del triángulo
panel_dentro_del_triangulo <- function(x, y, ancho, alto, base, altura) {
  esquina1 <- dentro_del_triangulo(x, y, base, altura)
  esquina2 <- dentro_del_triangulo(x + ancho, y, base, altura)
  esquina3 <- dentro_del_triangulo(x, y + alto, base, altura)
  esquina4 <- dentro_del_triangulo(x + ancho, y + alto, base, altura)
  return(esquina1 && esquina2 && esquina3 && esquina4)
}

#Función para empaquetar paneles en un techo triangular isósceles
empaquetar_triangular <- function(base, altura, a, b) {
#Verificar si alguna dimensión del panel es igual a 0
  if (a == 0 || b == 0 || base == 0 || altura == 0) {
    cat("No es posible colocar paneles: una de las dimensiones es 0.\n")
    return(NULL)
  }
#Verificar si el panel no cabe en el techo en ninguna orientación
  if ((a > base && a > altura) || (b > base && b > altura)) {
    cat("No es posible colocar paneles: el techo es más pequeño que los paneles en ambas orientaciones.\n")
    return(NULL)
  }
  
#Almacenar las posiciones y orientaciones de los paneles colocados
  paneles_colocados <- list()
  
#Verificar si un panel está en la misma posición que otro
  verificar_superposicion <- function(x0, y0, ancho, alto) {
    for (panel in paneles_colocados) {
      if (x0 < panel[1] + panel[3] && x0 + ancho > panel[1] &&
          y0 < panel[2] + panel[4] && y0 + alto > panel[2]) {
        return(TRUE)  # Hay superposición
      }
    }
    return(FALSE)  # No hay superposición
  }
  
#Función para colocar paneles en una fila o columna
  colocar_paneles <- function(x0, y0, ancho, alto) {
    if (panel_dentro_del_triangulo(x0, y0, ancho, alto, base, altura) && 
        !verificar_superposicion(x0, y0, ancho, alto)) {
      paneles_colocados <<- append(paneles_colocados, list(c(x0, y0, ancho, alto)))
      return(TRUE)
    }
    return(FALSE)
  }
  
#Iterar sobre el techo para colocar los paneles de manera eficiente
  y_actual <- 0
  while (y_actual + min(a, b) <= altura) {
    x_actual <- 0
    while (x_actual + min(a, b) <= base) {
#orientaciones (horizontal y vertical)
      if (colocar_paneles(x_actual, y_actual, a, b)) {
        x_actual <- x_actual + a
      } else if (colocar_paneles(x_actual, y_actual, b, a)) {
        x_actual <- x_actual + b
      } else {
        x_actual <- x_actual + 0.1  
      }
    }
    y_actual <- y_actual + min(a, b)
  }
  
#Devolver la lista de paneles colocados
  return(paneles_colocados)
}

#visualizar la disposición de los paneles en el techo triangular
visualizar_empaquetamiento_triangular <- function(base, altura, paneles_colocados) {
  plot(0, 0, xlim = c(0, base), ylim = c(0, altura), type = "n", 
       xlab = "Ancho del techo", ylab = "Altura del techo", 
       main = "Disposición de los paneles solares en el techo triangular", 
       asp = 1)  #Fijar la relación de aspecto a 1:1
  
#Dibujar el triángulo
  lines(c(0, base / 2, base, 0), c(0, altura, 0, 0), type = "l", col = "blue", lwd = 2)
  
  if (is.null(paneles_colocados) || length(paneles_colocados) == 0) {
#Mensajes
    text(base / 2, altura / 2, "No es posible colocar paneles", col = "red", cex = 1.5)
  } else {
#Plot paneles
    for (panel in paneles_colocados) {
      rect(panel[1], panel[2], panel[1] + panel[3], panel[2] + panel[4], col = "lightgreen", border = "darkgreen")
    }
  }
  
#Número total de paneles colocados fuera del gráfico
  mtext(paste("Paneles colocados:", length(paneles_colocados)), side = 3, line = 0.5, cex = 1.2, col = "black")
}

#Medidas
base <- 10  #Base del techo triangular
altura <- 6  #Altura del techo triangular
a <- 1      #Ancho del panel
b <- 2      #Alto del panel

#Empaquetar los paneles
paneles_colocados <- empaquetar_triangular(base, altura, a, b)

#Resultado
visualizar_empaquetamiento_triangular(base, altura, paneles_colocados)

#Cantidad de paneles colocados 
if (!is.null(paneles_colocados)) 
  cat("Cantidad de paneles colocados:", length(paneles_colocados), "\n")
  
