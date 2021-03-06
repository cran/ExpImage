% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_bands.R
\name{index_bands}
\alias{index_bands}
\title{Estimation of vegetation indices (Estimacao de indices de vegetacao).}
\usage{
index_bands(index="NDVI",red=NULL,nir=NULL,normalize=TRUE)
}
\arguments{
\item{index}{:Vegetation index to be estimated, default="NDVI" (Indice de vegetacao a ser estimado, default="NDVI").}

\item{red}{:Matrix with the red band (Matriz com a banda de vermelho).}

\item{nir}{:Matrix with the nir band (Matriz com a banda nir).}

\item{normalize}{:Logical value, if TRUE, the result will be normalized to vary between 0 and 1
(Valor logico, se for TRUE o resultado sera normatizado para variar entre 0 e 1).}
}
\value{
Return images size (Retorna o tamanho das imagens).
}
\description{
This function create vegetation indices
(Esta funcao cria indices de vegetacao).
}
\seealso{
\code{\link{gray_scale}} , \code{\link{read_image}}
}
\author{
Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
}
