empaquetar <- function(x, y, a, b) { #Corte de Guillotina
  
#CRITERIOS
#Verificar si alguna dimensión del panel es igual a 0
  if (a == 0 || b == 0 || x == 0 || y == 0) {
    cat("No es posible colocar paneles: una de las dimensiones es 0.\n")
    return(NULL)
  }
  
#Verificar si el panel no cabe en el techo en ninguna orientación
  if ((a > x && a > y) || (b > x && b > y)) {
    cat("No es posible colocar paneles: el techo es más pequeño que los paneles en ambas orientaciones.\n")
    return(NULL)
  }
  
#Almacenar las posiciones y orientaciones de los paneles colocados
  paneles_colocados <- list()
  
#Función para colocar paneles en una fila o columna
  colocar_fila_o_columna <- function(x0, y0, ancho, alto) {
#Intentar colocar paneles en orientación (a, b)
    if (a <= ancho && b <= alto) {
#Calcular cuántos paneles caben en esta fila o columna
      num_paneles_ancho <- floor(ancho / a)
      num_paneles_alto <- floor(alto / b)
      for (i in 0:(num_paneles_ancho - 1)) {
        for (j in 0:(num_paneles_alto - 1)) {
          paneles_colocados <<- append(paneles_colocados, list(c(x0 + i * a, y0 + j * b, a, b)))
        }
      }
#Devolver el espacio restante
      return(list(
        espacio_restante_1 = c(x0 + num_paneles_ancho * a, y0, ancho - num_paneles_ancho * a, alto),
        espacio_restante_2 = c(x0, y0 + num_paneles_alto * b, ancho, alto - num_paneles_alto * b)
      ))
    }
#Colocar paneles en orientación (b, a)
    if (b <= ancho && a <= alto) {
#Calcular cuántos paneles caben en esta fila o columna
      num_paneles_ancho <- floor(ancho / b)
      num_paneles_alto <- floor(alto / a)
      for (i in 0:(num_paneles_ancho - 1)) {
        for (j in 0:(num_paneles_alto - 1)) {
          paneles_colocados <<- append(paneles_colocados, list(c(x0 + i * b, y0 + j * a, b, a)))
        }
      }
#Devolver el espacio restante
      return(list(
        espacio_restante_1 = c(x0 + num_paneles_ancho * b, y0, ancho - num_paneles_ancho * b, alto),
        espacio_restante_2 = c(x0, y0 + num_paneles_alto * a, ancho, alto - num_paneles_alto * a)
      ))
    }
    return(NULL)
  }
  
#Colocar paneles en el techo
  espacio_actual <- list(c(0, 0, x, y))
  while (length(espacio_actual) > 0) {
    espacio <- espacio_actual[[1]]
    espacio_actual <- espacio_actual[-1]
    resultado <- colocar_fila_o_columna(espacio[1], espacio[2], espacio[3], espacio[4])
    if (!is.null(resultado)) {
      espacio_actual <- append(espacio_actual, list(resultado$espacio_restante_1, resultado$espacio_restante_2))
    }
  }
  
#Devolver la lista de paneles colocados
  return(paneles_colocados)
}

#Función para visualizar la disposición de los paneles en el techo
visualizar_empaquetamiento <- function(x, y, paneles_colocados) {
  plot(0, 0, xlim = c(0, x), ylim = c(0, y), type = "n", xlab = "Ancho del techo", ylab = "Alto del techo", main = "Disposición de los paneles solares en el techo")
  rect(0, 0, x, y, border = "blue", lwd = 2) # Dibujar el techo
  if (is.null(paneles_colocados) || length(paneles_colocados) == 0) {
    text(x / 2, y / 2, "No es posible colocar paneles", col = "red", cex = 1.5)
  } else {
    for (panel in paneles_colocados) {
      rect(panel[1], panel[2], panel[1] + panel[3], panel[2] + panel[4], col = "lightgreen", border = "darkgreen")
    }
  }

#Mostrar el número total de paneles colocados fuera del gráfico
mtext(paste("Paneles colocados:", length(paneles_colocados)), side = 3, line = 0.5, cex = 1.2, col = "black")
}

#Medidas
x <- 5  #Ancho del techo
y <- 3 #Alto del techo
a <- 0 #Ancho del panel
b <- 0  #Alto del panel
#Empaquetar los paneles
paneles_colocados <- empaquetar(x, y, a, b)

#Disposición resultante
visualizar_empaquetamiento(x, y, paneles_colocados)

#Cantidad de paneles colocados
if (!is.null(paneles_colocados)) {
  cat("Cantidad de paneles colocados:", length(paneles_colocados), "\n")
}

