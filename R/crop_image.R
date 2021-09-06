#'Esta funcao corta a imagem retirando as laterais nao desejadas. (This function
#' removes unwanted sides from the images.)
#'@description Esta funcao permite cortar a imagem (This function allows you to
#'  crop the image).
#'@usage crop_image(im,w,h,plot=TRUE,verbose=TRUE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@param w Deve ser um vetor contendo os numeros das colunas que permanecerao na
#'  imagem (It must be a vector containing the column numbers that will remain
#'  in the image).
#'@param h Deve ser um vetor contendo os numeros das linhas que permanecerao na
#'  imagem (It must be a vector containing the numbers of the lines that will
#'  remain in the image ).
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default)).
#'@param verbose Indica se sera apresentada (TRUE) ou nao (FALSE) (default) os
#'pontos de corte (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default) the points crop).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem cortada, apresentando apenas os  pixels
#'  selecionados (Returns a cropped image showing only selected pixels).
#'@seealso  \code{\link{edit_image}} ,  \code{\link{edit_imageGUI}}
#'@import EBImage
#'@importFrom EBImage is.Image display combine bwlabel readImage
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@importFrom graphics  lines locator
#'@export
#' @examples
#\donttest{
#'#library(ExpImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'
#'
#'##Cortar Imagem
#'
#'im3=crop_image(im,w =286:421,h=242:332,plot = TRUE)
#'
#'# intefacie grafica
#'\dontrun{
#'im2=crop_image(im)
#'}

#' @exportS3Method print ExpImage
#'




crop_image=function(im,w=NULL,h=NULL,plot=TRUE,verbose=TRUE){


  if((is.null(w)==F)|(is.null(h)==F)){
    if((mode(w)!="numeric")|is.null(h)) {stop("Vectors must be numeric.")}
    if((mode(h)!="numeric")|is.null(w)) {stop("Vectors must be numeric.")}
      if(is.Image(im)){im@.Data=im@.Data[w,h,]}
      if(is.matrix(im)){im=im[w,h]}
   }

   if(is.null(w)&is.null(h)){
     print("Clique sobre a imagem para cortar (Click on the image to crop)")
     if(is.Image(im)){plot(im)}
     if(is.matrix(im)){plot(as.Image((im)))}
     c=NULL
     for(i in 1:2){
      c0=locator(type = "p", n = 1, col = "red", pch = 22)
       c=rbind(c,c(c0$x,c0$y))

       if(i>1){
         lines(c[(i-1):i,],col="red")
       }
     }
     print(c)
       w=round(min(c[,1]),0):round(max(c[,1]),0)
       h=round(min(c[,2]),0):round(max(c[,2]),0)

     if(is.Image(im)){im@.Data=im@.Data[w,h,]}
     if(is.matrix(im)){im=im[w,h]}

     }



  if(plot==T){
    if(is.Image(im)){plot(im)}
    if(is.matrix(im)){plot(as.Image((im)))}

  }


  return(im)
}

print.ExpImage=function(x,...){
  if(is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}





