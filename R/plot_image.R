#'Esta funcao corta a imagem retirando as laterais nao desejadas. (This function
#' removes unwanted sides from the images.)
#'@description Esta funcao permite cortar a imagem (This function allows you to
#'  crop the image).
#'@usage plot_image(im)
#'@param im Este objeto deve conter uma imagem no formato do EBImage ou na forma
#'de uma matriz, no caso de imagem em escala de cinza (This
#'  object must contain an image in EBImage format).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Apresenta a imagem contida no objeto im.
#'@seealso  \code{\link{read_image}}
#'@import EBImage
#'@importFrom EBImage is.Image display combine bwlabel readImage
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'plot_image(im)
#'





plot_image=function(im){
  if(is.Image(im)){print(plot(im))}
  if(is.matrix(im)){plot(as.Image((im)))}
}
