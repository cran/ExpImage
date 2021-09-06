#' Image segmentation function (Funcao para a segmentacao de imagens).
#'
#' @description Esta funcao possibilita a segmentacao de imagens por meio de um
#'   limiar. Para isso, pode-se escolher um valor arbtrario ou considerar o
#'   valor estabelecido pelo metodo otsu.
#' @usage
#'   segmentation(img.band,treshold="otsu",selectHigher=TRUE,
#'   fillHull=FALSE,fillBack=FALSE,TargetPixels="all",plot=FALSE)
#' @param img.band    :Este objeto deve ser obrigatoriamente uma matriz contendo
#'   valores entre 0 a 1 correspondente a imagem em escala de cinza).
#' @param treshold    : E um valor numerico entre 0 e 1 a ser considerado como
#'   limiar. O usuario pode tambem usar o argumento "ostu", caso queira
#'   considerar o limiar estabelecido por essa metodologia.
#' @param selectHigher    :Este argumento deve receber as palavras TRUE ou
#'   FALSE. TRUE e quando se quer selecionar pixels de valores  maiores que o
#'   limiar. FALSE quando se deseja selecionar valores menores.
#' @param fillHull    :Este argumento deve receber a palavra TRUE quando se
#'   pretende desconsiderar valores vazios dentro do foreground, caso contrario
#'   FALSE.
#' @param fillBack    :Este argumento deve receber a palavra TRUE quando se
#'   pretende desconsiderar valores vazios dentro do background, caso contrario
#'   FALSE.
#' @param TargetPixels    :Quando se pretende segmentar todos os pixeis da
#'   imagem deve considerar a palavra "all" (Default). Se a segmentacao deva ser
#'   feita apenas para um conjunto de pixels, este deve ser apresentada em uma
#'   matriz contendo o valor 1 para os pixeis de interesse e 0 para os demais.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada.
#' @return Imagem segmentada
#' @seealso  \code{\link{segmentation_logit}}
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@export
#' @examples
#'\donttest{
#'############################################################################
#'#Obtendo o numero de ovos em uma folha
#'############################################################################
#'
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'##mostrar imagem
#'plot(im)
#'

#'
#'#Selecionando o melhor indice para a segmentacao da folha
#'r=gray_scale(im,method = "r",plot=TRUE)
#'g=gray_scale(im,method = "g",plot=TRUE)
#'b=gray_scale(im,method = "b",plot=TRUE)
#'
#'
#'#O canal azul possibilita maior contraste
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada=segmentation(b,treshold = 0.30,fillHull = TRUE,
#'selectHigher = FALSE,plot=TRUE)
#'
#'im2=extract_pixels(im,target =MatrizSegmentada,valueTarget =1,
#'valueSelect = c(0,0,0),plot=TRUE )
#'
#'
#'#Selecionando o melhor indice para a segmentacao dos ovos
#'r=gray_scale(im2,method = "r",plot=TRUE)
#'g=gray_scale(im2,method = "g",plot=TRUE)
#'b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'#O canal Azul proporciona melhor segmentacao
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada2=segmentation(b,treshold = 0.50,fillHull = TRUE,
#'selectHigher = TRUE,plot = TRUE)
#'
#'Medidas=measure_image(MatrizSegmentada2)
#'Medidas$ObjectNumber
#'
#'#Ver a mascara sobre os ovos na foto
#'im3=mask_pixels(im,MatrizSegmentada2==1,plot=TRUE)
#'}

segmentation=function(img.band,treshold="otsu",selectHigher=TRUE,fillHull=FALSE,
                      fillBack=FALSE,TargetPixels="all", plot=FALSE){
  #library(EBImage)
  if(isFALSE(is.matrix(TargetPixels))){
    b=(img.band)
    if(treshold=="otsu"){ts=EBImage::otsu(b);message(
      paste("The treshold by Otsu melhod is (O valor do limiar pelo etodo otsu e):",round(ts,4)))}
    if(treshold!="otsu"){ts=treshold}
    if(isTRUE(selectHigher)){MatrizSegentada=b>ts}
    if(isFALSE(selectHigher)){MatrizSegentada=b<ts}
  }


  if(isTRUE(is.matrix(TargetPixels))){
    b=img.band
    if(treshold=="otsu"){ts=suppressWarnings(otsu(matrix(b[TargetPixels],ncol=2)))
    message(paste("The treshold by Otsu melhod is (O valor do limiar pelo etodo otsu e):",round(ts,4)))}
    if(treshold!="otsu"){ts=treshold}
    MatrizSegentada=TargetPixels*1
    if(isTRUE(selectHigher)){MatrizSegentada=b>ts}
    if(isFALSE(selectHigher)){MatrizSegentada=b<ts}  }



  if(isTRUE(fillHull)){MatrizSegentada=bwlabel(MatrizSegentada);MatrizSegentada=fillHull(MatrizSegentada)}
  if(isTRUE(fillBack)){MatrizSegentada=bwlabel(MatrizSegentada);MatrizSegentada=fillHull(MatrizSegentada==0);MatrizSegentada==0}

  MatrizSegentada[MatrizSegentada>0]=1
  MatrizSegentada=MatrizSegentada*1





  if(plot==T){plot(as.Image((MatrizSegentada)))}
  return(MatrizSegentada)
}
