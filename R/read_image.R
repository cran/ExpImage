#' Function to import an image (Funcao para importa uma imagem).
#'
#' @description Esta funcao importa uma imagem.
#' @usage read_image(file,plot=FALSE)

#' @param file    :Nome do arquivo ou endereco da imagem.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Importa uma imagem.
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(1),plot=TRUE)
#}
#'@export


read_image=function(file,plot=FALSE){
  n=unlist(strsplit(file, "[.]"))
  if(n[length(n)]=="tif"){
    im2=raster::stack(file)
    arr=array(NA,c(im2@ncols,im2@nrows, length(im2@layers)))
    for(i in 1:length(im2@layers)){
      arr[,,i]=t(as.matrix(raster(im2,i))/255)
    }

    im3=EBImage::as.Image(arr)
    EBImage::colorMode(im3)=2

  }

  if(n[length(n)]!="tif"){
    im3=EBImage::readImage(file)
  }


  if(plot==T){
    plot_image(im3)}


  return(im3)
}


