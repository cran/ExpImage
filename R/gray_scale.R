#' Function to get a grayscale image from a color image (Funcao para a obtencao
#' de uma imagem em escala de cinza a partir de uma imagem colorida)
#'
#' @description Function to get a grayscale image from a color image (Esta
#'   funcao permite a obtencao de uma imagem em escala de cinza a partir de uma
#'   imagem colorida).
#' @usage gray_scale(im,method="r",plot=FALSE)
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param method    : Indicates the method for obtaining the gray scale (Este
#'   objeto indica o metodo para a obtencao da escala de cinza).:\cr "r" =
#'   extrair a banda de vermelho\cr "g" = extrair a banda de verde\cr "b" =
#'   extrair a banda de azul\cr "rg" = considera a media da banda de vermelho e
#'   verde: (r+g)/2\cr "rb" = considera a media da banda de vermelho e azul:
#'   (r+b)/2\cr "gb" = considera a media da banda de verde e azul: (g+b)/2\cr
#'   "gbb" = considera a media das 3 bandas: (r+g+b)/3\cr
#'
#' @param plot    :This object must contain an image in EBImage format (Indica
#'   se sera apresentada (TRUE) ou nao (FALSE) (default) a imagem segmentada).

#' @return Returns an image with text over each object in the image (Retorna uma
#'   imagem com um texto sobreposto a cada objeto na imagem)
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'##mostrar imagem
#'plot(im)
#'

#'
#'#Extraindo as camadas R, G e B
#'r=gray_scale(im,method = "r",plot=TRUE)
#'g=gray_scale(im,method = "g",plot=TRUE)
#'b=gray_scale(im,method = "b",plot=TRUE)
#}
#'@export


gray_scale=function(im,method="r",plot=FALSE){
  #Separar a imagem em bandas


  normatizar=function(MAT){
    MAT=MAT-min(c(MAT))
    id=1/max(MAT)
    MAT=MAT*id
    return(MAT)
  }

  r=im@.Data[,,1]
  g=im@.Data[,,2]
  b=im@.Data[,,3]

  if(method=="r"){imm=r}
  if(method=="g"){imm=g}
  if(method=="b"){imm=b}
  if(method=="rg"){imm=(r+g)/2}
  if(method=="rb"){imm=(r+b)/2}
  if(method=="gb"){imm=(g+b)/2}
  if(method=="rgb"){imm=(r+g+b)/3}

  if(method=="r/rgb"){imm=normatizar(r/(max(c(r))+max(c(g))+max(c(b))))}
  if(method=="g/rgb"){imm=normatizar(g/(max(c(r))+max(c(g))+max(c(b))))}
  if(method=="b/rgb"){imm=normatizar(b/(max(c(r))+max(c(g))+max(c(b))))}

  if(plot==T){print(display(imm))}

  return(imm)

}
