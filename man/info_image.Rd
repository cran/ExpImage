% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info_image.R
\name{info_image}
\alias{info_image}
\title{Image information  (Informacoes sobre as imagens).}
\usage{
info_image(im)
}
\arguments{
\item{im}{:Object containing an image (Objeto contendo a uma imagem).}
}
\value{
Importa uma imagem.
}
\description{
This function returns information about the image
(Esta funcao retorna informacoes sobre a imagem).
}
\examples{
#Carregar imagem de exemplo
im=read_image(example_image(1),plot=TRUE)
info_image(im)
}
\seealso{
\code{\link{crop_image}} , \code{\link{edit_image}}
}
\author{
Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
}
