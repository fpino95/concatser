#' @title Analisis descriptivo a un par de variables continuas (x vs y)
#' @description  Esta funcion realiza el analisis descriptivo a un par de variables continuas (x vs y), mostrando: Grafico x vs y, Regresion lineal y ~ x, Media, Cuartiles, Prueba de normalidad a cada variable, Grafico de distribucion de cada variable.
#' @param data DataFrame con ambas variables continuas. Deben ser ingresadas con numericas (enteras o decimales). La primera columna corresponde a la variable x (independiente), la segunda columna a la variable y (dependiente).
#' @param titulo_grafico Titulo del grafico principal. Si se deja vacio entonces se asigna "Grafico x vs y". Se ingresa como cadena de caracteres.
#' @param solo_grafico TRUE o FALSE, determina si quiere verse solo el grafico general o si tambien se imprime la informacion descriptiva. TRUE imprime solamente el grafico, FALSE imprime el grafico y la informacion descriptiva.
#' @return Graficos y resultados
#' @export desc_continuos
#' @import dplyr plotly
#' @examples
#' desc_continuos(data = iris[, c(1,3)], titulo_grafico = 'Sepal vs Petal')
#' desc_continuos(data = iris[, c(1,3)])


desc_continuos <- function(data, titulo_grafico = "Grafico",
                           solo_grafico = FALSE){
  datos <- data
  # si no se ingresa nombre al grafico
  if(titulo_grafico == 'Grafico'){
    titulo_grafico <- paste0('Grafico ', colnames(datos)[1], ' vs ', colnames(datos)[2])
  }

  #regresion para la tendencia
  li_reg <- lm(datos[,2] ~ datos[,1]) # "y" es datos[,2]

  # grafico x vs y (ingresado como data frame)
  fig <- plot_ly(x = datos[,1], y = datos[,2], name = 'Datos',
                 type = 'scatter', mode = 'markers') %>%
    add_trace(y = li_reg$fitted.values, name = 'Regresion Lineal',
              mode = 'lines') %>%
    layout(title = titulo_grafico,
           xaxis = list(title = colnames(datos)[1]),
           yaxis = list(title = colnames(datos)[2])) %>%
    layout(hovermode = 'compare')

  print(fig)

  if(!solo_grafico){

    # significancia regresion lineal
    cat('Resultado Regresion lineal: \n')
    print(summary(li_reg))
    cat(paste0('datos[, 1] corresponde a ', colnames(datos)[1],
                 ', en este caso la variable independiente (x) \n'))

    # medidas de tendencia central
    cat(paste0('Medidas de tendencia central para ', colnames(datos)[1], ' son: \n'))
    print(summary(datos[,1]))

    cat(paste0('Medidas de tendencia central para ', colnames(datos)[2], ' son: \n'))
    print(summary(datos[,2]))

    # prueba de normalidad para ambas vbles
    cat(paste0('Prueba de normalidad ', colnames(datos)[1], ', valor-p prueba de Shapiro Wilk: \n'))
    normalidad <- shapiro.test(datos[,1])
    cat(paste0(round(normalidad$p.value, digits = 4), ' \n'))
    if(normalidad$p.value >= 0.05){cat('Se supone normalidad \n')}
    if(normalidad$p.value < 0.05){cat('Se evidencia no normalidad \n')}

    cat(paste0('Prueba de normalidad ', colnames(datos)[2], ', valor-p prueba de Shapiro Wilk: \n'))
    normalidad <- shapiro.test(datos[,2])
    cat(paste0(round(normalidad$p.value, digits = 4), ' \n'))
    if(normalidad$p.value >= 0.05){cat('Se supone normalidad \n')}
    if(normalidad$p.value < 0.05){cat('Se evidencia no normalidad \n')}

    # Graficos de distribucion
    x11()
    par(mfrow=c(2,1))
    plot(density(datos[, 1], adjust = 1),
         main = paste0('Densidad ', colnames(datos)[1]))
    plot(density(datos[, 2], adjust = 1),
         main = paste0('Densidad ', colnames(datos)[2]))
    par(mfrow=c(1,1))
  }

}


