% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick_palette.R
\name{pick_palette}
\alias{pick_palette}
\title{Obter uma paleta de cores com cliques (Get a color palette with clicks ).}
\usage{
pick_palette(im,palette.previous=NULL,percent.pick=0.01)
}
\arguments{
\item{im}{Este objeto deve conter uma imagem no formato do EBImage (This
object must contain an image in EBImage format ).}

\item{palette.previous}{Este objeto pode conter uma imagem no formato do EBImage
com parte da paleta de cores obtida previamente (This
 object can contain an image in EBImage format whith a color palette).}

\item{percent.pick}{E um valor variando entre 0 e 1. Quanto menor for o valor, menor
sera a area capturada com os cliques. (And a value ranging between 0 and 1. The smaller
the value, the smaller the area captured with the clicks will be).}
}
\value{
Esta funcao retorna uma paleta de cores com os pixels selecionados.
(This function returns a color palette with selected pixels).
}
\description{
Esta funcao retorna uma paleta de cores com os pixels selecionados.
(This function returns a color palette with selected pixels).
}
\examples{
\dontrun{
#library(ExpImage)
#library(EBImage)
#Carregar imagem de exemplo
im=read_image(example_image(2),plot=TRUE)
pick_palette(im)
}
}
\seealso{
\code{\link{segmentation_logit}}, \code{\link{pick_color}}
}
\author{
Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
}
