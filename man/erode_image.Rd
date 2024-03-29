% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erode_image.R
\name{erode_image}
\alias{erode_image}
\title{Funcao para eliminar as bordas dos objetos.}
\usage{
erode_image(im,n=1,plot=TRUE)
}
\arguments{
\item{im}{Este objeto deve conter uma imagem binaria em uma matriz.}

\item{n}{Numero de vezes que deseja-se a exclusao das bordas.}

\item{plot}{Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
imagem segmentada.}
}
\value{
Retorna uma imagem excluindo-se suas bordas.
}
\description{
Esta funcao permite excluir as bordas de objetos nas imagens.
}
\examples{
#Abrindo uma imagem de exemplo
im=read_image(example_image(10),plot=TRUE)
b=gray_scale(im,method = "b",plot=TRUE)
raiz=segmentation(b,threshold = 0.62,selectHigher = TRUE,plot=TRUE)
raiz2=erode_image(raiz,n = 1,plot = TRUE)

}
\seealso{
\code{\link{edit_image}} ,  \code{\link{dilate_image}}
}
\author{
Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
}
