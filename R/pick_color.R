#'Selecionar os valores de RGB em um pixel (Selecting RGB values in a pixel).
#'
#'@description Esta funcao retorna o valor de R, G e B no pixel selecionado.
#' (This function returns the value of R, G and B at the selected pixel ).
#'@usage pick_color(im)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Esta funcao retorna o valor de R, G e B no pixel selecionado.
#' (This function returns the value of R, G and B at the selected pixel ).
#'@seealso  \code{\link{segmentation_logit}}
#'@import EBImage
#'@importFrom EBImage is.Image display combine bwlabel readImage
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'\donttest{
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'pick_color(im)
#'}

#' @exportS3Method print ExpImage
#'




pick_color=function(im){
  print("Clique sobre a imagem (Click on the image)")
plot(im)
c0=locator(type = "p", n = 1, col = "red", pch = 22)
im@.Data[c0$x,c0$y,]
}
