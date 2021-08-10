#' Function to open an image (Funcao para abrir uma imagem).
#'
#' @description Esta funcao abre uma imagem.
#' @usage read_image(file,plot=FALSE)

#' @param file    :Nome do arquivo ou endereco da imagem.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem editada
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Abre uma imagem.
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(1),plot=TRUE)
#}
#'@export


read_image=function(file,plot=FALSE){
  im2=readImage(file)
if(plot==T){plot(im2)}
  return(im2)
}
