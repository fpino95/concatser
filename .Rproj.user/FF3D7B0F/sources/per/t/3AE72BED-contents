#' @title Analisis descriptivo a serie de tiempo
#' @description  Esta funcion realiza el analisis descriptivo a una serie de tiempo, mostrando: Grafico de la serie, Tendencia, Media, Cuartiles, Prueba de normalidad, Prueba ruido blanco y Grafico distribucion de los datos.
#' @param data Serie de tiempo, debe ser ingresada como vector. Se recomienda usar as.vector() para ingresar la serie.
#' @param nombre_serie Nombre de la serie de tiempo. Si se deja vacio entonces se llamara "Serie". Se ingresa como cadena de caracteres.
#' @param titulo_grafico Titulo del grafico principal de la serie. Si se deja vacio entonces se toma el mismo nombre de la serie. Se ingresa como cadena de caracteres.
#' @param solo_grafico TRUE o FALSE, determina si quiere verse solo el grafico general o si tambien se imprime la informacion descriptiva. TRUE imprime solamente el grafico, FALSE imprime el grafico y la informacion descriptiva.
#' @return Graficos y resultados
#' @export descriptiva_serie
#' @import dplyr plotly sarima
#' @examples
#' descriptiva_serie(data = as.vector(iris$Sepal.Length),
#'                   nombre_serie = 'Largo del Sepalo',
#'                   titulo_grafico = 'Longitud Sepalo')


descriptiva_serie <- function(data, nombre_serie = 'Serie',
                              titulo_grafico = nombre_serie,
                              solo_grafico = FALSE){
  datos <- data.frame(secuencia = seq(1, length(data), by = 1),
                      data)

  li_reg <- lm(datos[,2] ~ datos[,1])

  fig <- plot_ly(x = datos[,1], y = datos[,2], name = 'Serie',
                 type = 'scatter', mode = 'lines') %>%
    add_trace(y = li_reg$fitted.values, name = 'Tendencia',
              mode = 'lines') %>%
    layout(title = titulo_grafico,
           xaxis = list(title = 'Secuencia'),
           yaxis = list(title = nombre_serie)) %>%
    layout(hovermode = 'compare')

  print(fig)

  if(!solo_grafico){

    cat(paste0('Medidas de tendencia central para ', colnames(datos)[2], ' son: \n'))
    print(summary(datos[,2]))

    cat('Prueba de normalidad, valor-p prueba de Shapiro Wilk: \n')

    normalidad <- shapiro.test(datos[,2])
    cat(paste0(round(normalidad$p.value, digits = 4), ' \n'))

    if(normalidad$p.value >= 0.05){cat('Se supone normalidad \n')}
    if(normalidad$p.value < 0.05){cat('Se evidencia no normalidad \n')}

    # grafico de densidad
    plot(density(datos[, 2], adjust = 1),
          main = paste0('Densidad ', nombre_serie))

    # prueba aletoriedad - RUIDO BLANCO

    x <- datos[, 2]

    x.acf <- autocorrelations(x)


    x.iid <- whiteNoiseTest(x.acf, h0 = "iid", nlags = seq(1, nrow(datos), by = 1), x = x,
                            method = "LiMcLeod")

    test_white <- data.frame(x.iid$test)
    test_white <- na.omit(test_white)

    if(sum(test_white$pvalue > 0.05) == nrow(test_white)){
      cat('Definitivamente es ruido blanco \n')
    }

    if((sum(test_white$pvalue > 0.05) < nrow(test_white)) &
       (sum(test_white$pvalue > 0.05) >= nrow(test_white) / 2)){
      cat('Puede ser ruido blanco \n')
    }

    if((sum(test_white$pvalue > 0.05) < nrow(test_white) / 2) &
       (sum(test_white$pvalue > 0.05) > 0)){
      cat('Puede no ser ruido blanco \n')
    }

    if(sum(test_white$pvalue > 0.05) == 0){
      cat('Definitivamente NO es ruido blanco \n')
    }
  }

}

# antes de oxigenizar rm(list = ls())
# ejecutar roxygen2::roxygenise(), eliminando namespace y los .Rd en man
# previamente

# para instalarlo en github, hacerlo con github desktop, subir la carpeta
# del paquete como repositorio

# install_github("fpino95/descriptiva1") para instalarlo desde github

# Guia de Instalacion
#
# Instalar libreria "remotes". Cargar la libreria.
#
# Verificar que se tengan instaladas las librerias "dplyr", "plotly" y "sarima".
# Si no se tiene instalada alguna de ellas, entonces instalarla.
#
# Ejecutar el comando install_github("fpino95/descriptiva1") y esperar que se instale.
#
# En caso de error, puede ser por falta de alguna de las librerias previamente mencionadas.
#
# Una vez instalada, la libreria que disponible completamente y se pueden consultar las funciones a traves de help en RStudio.
#
# Las 3 funciones son: "desc_categoricos", "desc_continuos" y "descriptiva_serie".
