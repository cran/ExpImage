#' Creates a mask over the background or foreground (Cria uma mascara sobre o
#' background ou foreground)
#'
#' @description Esta funcao permite criar mascara sobre os pixels
#'   correspondentes ao background ou foreground
#' @usage mask_pixels(im,TargetPixels,TargetPixels2=NULL,plot=FALSE)

#' @param im    :Este objeto deve conter uma imagem no formato do EBImage.
#' @param TargetPixels    : Este objeto deve ser obrigatoriamente uma matriz
#'   binaria, contendo os valores 0 (pixels do background) ou 1 (pixels do
#'   foreground)).
#' @param TargetPixels2    : Este objeto pode ter o valor "NULL" caso haja
#'   apenas uma mascara a ser destacada sobre a imagem. Se quiser usar duas
#'   mascaras, neste objeto deve ter obrigatoriamente uma matriz binaria,
#'   contendo os valores 0 (pixels do background) ou 1 (pixels do foreground)).

#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada.

#' @return Retorna uma imagem com uma mascara sobre os  pixels selecionados.
#@seealso  \code{\link{segmentation_logit}}

#' @examples
#\donttest{
#'###########################################################################
#'#Estimar a area atacada por doenca no tomateiro
#'###########################################################################
#'
#'   im=read_image(example_image(ex=7))
#'   plot(im)
#'
#'   #Selecionando o melhor indice para a segmentacao da folha
#'   r=gray_scale(im,method = "r",plot=TRUE)
#'   g=gray_scale(im,method = "g",plot=TRUE)
#'   b=gray_scale(im,method = "b",plot=TRUE)
#'
#'   #O limiar pode ser um valor escolhido aleatoriamente
#'   MatrizSegentada=segmentation(b,treshold = 0.5,fillHull = FALSE,plot=TRUE)
#'

#'   #O limiar tambem pode ser estabelecido pelo metodo de otsu
#'   MatrizSegentada2=segmentation(b,treshold = "otsu",fillHull = TRUE
#'   ,selectHigher= FALSE, plot=TRUE)
#'
#'
#'   #Selecionar na imagem apenas os pixeis desejaveis (Folha)
#'   im2=extract_pixels(im,target=MatrizSegentada2,valueTarget=TRUE,
#'   valueSelect=c(r=1,g=1,b=1),plot=TRUE)
#'
#'   ################################################################
#'   #Selecionando o melhor indice para a segmentacao da doenca
#'   r=gray_scale(im2,method = "r",plot=TRUE)
#'   g=gray_scale(im2,method = "g",plot=TRUE)
#'   b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'   MatrizSegmentada3=segmentation(g,treshold = 0.3,selectHigher = FALSE,
#'   fillHull =TRUE,plot=TRUE)
#'
#'
#'   #Como pode-se obsevar, a segmentacao por limiar nao e possivel. Entao vamos
#'   #usar paletas de cores
#'   folha=read_image(example_image(ex=8))
#'   doenca=read_image(example_image(ex=9))
#'
#'   DoencaSeg=segmentation_logit(im,foreground = doenca,background =
#'   folha,sample = 2000,fillHull = TRUE,TargetPixels =MatrizSegentada2==1
#'   ,plot=TRUE)
#'
#'   im3=mask_pixels(im2,TargetPixels=DoencaSeg==1)
#'    plot(im3)
#'
#'   ii=join_image(im,im3,plot=TRUE)
#'
#'
#'   #Porcentagem da area lesionada.
#'
#'   100*(sum(DoencaSeg)/sum(MatrizSegentada2))
#}
#'@export

mask_pixels=function(im,TargetPixels,TargetPixels2=NULL,plot=FALSE){
  TargetPixels==1

  r=im@.Data[,,1]
  g=im@.Data[,,2]
  b=im@.Data[,,3]

  r[TargetPixels]=1
  g[TargetPixels]=0
  b[TargetPixels]=0

  im@.Data[,,1]=r
  im@.Data[,,2]=g
  im@.Data[,,3]=b

  if(isFALSE(is.null(TargetPixels2))){
    TargetPixels2==1
    r=im@.Data[,,1]
    g=im@.Data[,,2]
    b=im@.Data[,,3]

    r[TargetPixels2]=0
    g[TargetPixels2]=0
    b[TargetPixels2]=1

    im@.Data[,,1]=r
    im@.Data[,,2]=g
    im@.Data[,,3]=b
  }

  if(plot==T){plot(im)}


  return(im)
}

crop.image=function(im,w,h,plot=TRUE){
  if(is.Image(im)){im@.Data=im@.Data[w,h,]}
  if(is.matrix(im)){im=im[w,h]}

  if(plot==T){
    if(is.Image(im)){plot(im)}
    if(is.matrix(im)){display(im)}

  }
  return(im)
}
