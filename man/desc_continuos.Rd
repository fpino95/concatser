% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc_continuos.R
\name{desc_continuos}
\alias{desc_continuos}
\title{Analisis descriptivo a un par de variables continuas (x vs y)}
\usage{
desc_continuos(data, titulo_grafico = "Grafico", solo_grafico = FALSE)
}
\arguments{
\item{data}{DataFrame con ambas variables continuas. Deben ser ingresadas con numericas (enteras o decimales). La primera columna corresponde a la variable x (independiente), la segunda columna a la variable y (dependiente).}

\item{titulo_grafico}{Titulo del grafico principal. Si se deja vacio entonces se asigna "Grafico x vs y". Se ingresa como cadena de caracteres.}

\item{solo_grafico}{TRUE o FALSE, determina si quiere verse solo el grafico general o si tambien se imprime la informacion descriptiva. TRUE imprime solamente el grafico, FALSE imprime el grafico y la informacion descriptiva.}
}
\value{
Graficos y resultados
}
\description{
Esta funcion realiza el analisis descriptivo a un par de variables continuas (x vs y), mostrando: Grafico x vs y, Regresion lineal y ~ x, Media, Cuartiles, Prueba de normalidad a cada variable, Grafico de distribucion de cada variable.
}
\examples{
desc_continuos(data = iris[, c(1,3)], titulo_grafico = 'Sepal vs Petal')
desc_continuos(data = iris[, c(1,3)])
}
