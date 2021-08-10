#'Function for skeletonization in images (Esta funcao faz a skeletonizacao em
#'imagens).

#'@description Esta funcao permite fazer a skeletonizacao em imagens.
#'@usage skeletonize_image(x,plot=FALSE)
#'@param x    :Este objeto deve conter uma imagem em uma matriz binaria.
#'@param plot    :Se forigual a TRUE a imagem sera plotada.
#'#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem com os objetos skeletonizados.
#'@seealso  \code{\link{thinning_image}}
#'@import EBImage
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#\donttest{
#' im=read_image(example_image(10),plot=TRUE)
#' im2=segmentation(im@.Data[,,1],plot = TRUE)
#' T1=skeletonize_image(im2,plot = TRUE)
#' T2=thinning_image(im2,plot = TRUE)
#}
#'@export


skeletonize_image <- function(x,plot=FALSE){
  s <- matrix(1, nrow(x), ncol(x))
  skel <- matrix(0, nrow(x), ncol(x))
  kern <- makeBrush(3, shape="diamond")
  while(max(s)==1){
    k <- opening(x, kern)
    s <- x-k
    skel <- skel | s
    x <- erode(x, kern)
  }
  if(plot==T){print(display(skel))}
  return(skel)
}
