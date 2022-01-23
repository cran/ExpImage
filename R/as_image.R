#' Transform matrix into image (Transformar matriz em imagem).
#'
#' @description This function transform matrix into image  (Esta funcao transforma matriz em imagem).
#' @usage as_image(img)

#' @param img    :Object image  (Objeto com uma imagem).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns an object of type image (Retorna um objeto do tipo imagem).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#' im1=read_image(example_image(2),plot=TRUE)
#' r=gray_scale(im = im1,method = "r")
#' R=as_image(r)
#
#'@export


as_image=function(img){
  EBImage::as.Image(img)
}
