% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_image.R
\name{write_image}
\alias{write_image}
\title{Function to save an image (Funcao para salvar uma imagem).}
\usage{
write_image(x, files, type, quality = 100, bits.per.sample, compression = "none")
}
\arguments{
\item{x}{an Image object or an array.}

\item{files}{a character vector of file names or URLs.}

\item{type}{image type (optional). Supported values are: jpeg, png, and tiff. If missing, file format is automatically determined by file name extension.}

\item{quality}{a numeric ranging from 1 to 100 (default) controlling the quality of the JPEG output.}

\item{bits.per.sample}{a numeric scalar specifying the number of bits per sample (only for tiff files). Supported values are 8 and 16.}

\item{compression}{the desired compression algorithm (only for tiff files). For a list of supported values consult the documentation of the writeTIFF function from the tiff package.}
}
\value{
Salva uma imagem.
}
\description{
Esta funcao salva uma imagem.A funcao e uma adaptacao do `writeImage` do pacote `EBImage`
}
\examples{
\dontrun{
#Carregar imagem de exemplo
im=read_image(example_image(1),plot=TRUE)
im2=resize_image(im,w=1000,plot=TRUE)
im3=crop_image(im2,w =200:650,h=100:450,plot = TRUE)
imb=edit_image(im3,brightness = 0.1,contrast = 1.7,gamma  = 1.2)
write_image(x = imb,files = "Test.jpg")
}
}
\seealso{
\code{\link{read_image}}
}
\author{
Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
}
