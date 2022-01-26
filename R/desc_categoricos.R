#' @title Analisis descriptivo a variables categoricas
#' @description  Esta funcion realiza un grafico resumen (proporciones) a un dataframe con variables categoricas. En caso de ingresar variables continuas se llevaran a character y seran tratadas como categoricas.
#' @param data DataFrame con las variables categoricas a visualizar.
#' @return Graficos y resultados
#' @export desc_categoricos
#' @import dplyr plotly
#' @examples
#' desc_categoricos(data = CO2[, 2:3])
#' desc_categoricos(data = mtcars[, c(2, 8:11)])
#' desc_categoricos(data = cars)


desc_categoricos <- function(data){
  datos <- data
  for (i in 1:ncol(datos)) {
    columna_i <- as.character(datos[, i])

    # agregar las categorias y ordenarlas por conteo
    datos_i <- data.frame(columna_i, conteo = 1)
    datos_i_back <- datos_i

    datos_i <- datos_i %>% group_by(columna_i) %>%
      summarise(conteo = sum(conteo, na.rm = T)) %>%
      arrange(conteo)
    datos_i <- as.data.frame(datos_i)

    colnames(datos_i) <- c(colnames(datos)[i], "conteo")
    #colnames(datos_i_back) <- c(colnames(datos)[i], "conteo")

    datos_i$Proporcion <- round(100*datos_i$conteo/sum(datos_i$conteo), digits = 2)

    # si tiene mas de 10 categorias, hacer grafico de barras, si tiene menos
    # de 10, hacer grafico de torta
    if(nrow(datos_i) <= 10){
      fig <- plot_ly(labels = datos_i_back[,1], values = datos_i_back[,2], type = 'pie')
      fig <- fig %>% layout(title = paste0(colnames(datos)[i]),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      print(fig)
    }

    # si tiene mas de 20 categorias, hacer grafico de barras
    if(nrow(datos_i) > 10){
      fig <- plot_ly(
        x = datos_i[, 1],
        y = datos_i[, 3],
        color = datos_i[, 1],
        type = "bar"
      )
      fig <- fig %>% layout(title = paste0(colnames(datos)[i]),
                            xaxis = list(title = 'Categorias',
                                         categoryorder = "total descending"),
                            yaxis = list(title = 'Proporcion'))

      print(fig)
    }


  }

  cat('Termina satisfactoriamente, graficos en Viewer \n')
}


