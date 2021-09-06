#'Esta funcao rotaciona a imagem. (This function rotates the image .)
#'@description Esta funcao permite rotacionar a imagem (This function allows
#'rotate the image).
#'@usage rotate_image(im,angle=NULL,BGcolor=c(0,0,0),plot=TRUE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@param angle Valor em graus (Degree value).
#'@param BGcolor Vetor com os valores que preencherao o background (Vector with
#' the values that will fill the background).
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default)).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem rotacionada (Returns a rotated image).
#'@seealso  \code{\link{edit_image}}
#'@import EBImage
#'@importFrom EBImage is.Image display combine bwlabel readImage
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#\donttest{
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'
#'##rotacionar a imagem
#'im2=rotate_image(im,angle=80)
#'im2=rotate_image(im,angle=80,BGcolor=c(1,1,1))
#}

#' @exportS3Method print ExpImage
#'




rotate_image=function(im,angle=NULL,BGcolor=c(0,0,0),plot=TRUE){

  #rotacionar=function(im,angle,BGcolor,plot){
      if(is.Image(im)){
        im2=rotate(im,angle = angle)
        id=(im2@.Data[,,1]==0)&(im2@.Data[,,2]==0)&(im2@.Data[,,3]==0)
        im2@.Data[,,1][id]=BGcolor[1]
        im2@.Data[,,2][id]=BGcolor[2]
        im2@.Data[,,3][id]=BGcolor[3]
        if(plot==TRUE){plot(im2)}
        return(im2)
      }
      if(is.matrix(im)){
        im0=as.Image(im)
        im2=rotate(im0,angle = angle)
        id=(im2@.Data[,]==0)
        im2@.Data[id]=BGcolor[1]
        im3=im2@.Data
        if(plot==TRUE){plot(as.Image((im3)))}
        return(im3)
      }
 #  }
 #
 #
 #  if(is.null(angle)){
 #    sample.norm<-function(){
 #      refresh.code<-function(...){
 #        mu<-slider(no=1);
 #      imxx=  rotacionar(im=im,angle=mu,BGcolor = BGcolor,plot=TRUE)
 #
 #      }
 #      mu=slider(refresh.code,sl.names=c("Angulo (angle)"),
 #             sl.mins=c(0),sl.maxs=c(360),sl.deltas=c(.1),sl.defaults=c(0),)
 #
 #    }
 # a=sample.norm()
 #
 #  #eeturn(rotacionar(im=im,angle=mu,BGcolor = BGcolor,plot=F))
 #
 #  }


   }



print.ExpImage=function(x,...){
  if(is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}
