#'Esta funcao corta a imagem retirando as laterais nao desejadas. (This function
#' removes unwanted sides from the images.)
#'@description Esta funcao permite cortar a imagem (This function allows you to
#'  crop the image).
#'@usage crop_image(im,w=NULL,h=NULL,segmentation=NULL,plot=TRUE,verbose=FALSE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format).
#'@param w Deve ser um vetor contendo os numeros das colunas que permanecerao na
#'  imagem (It must be a vector containing the column numbers that will remain
#'  in the image).
#'@param h Deve ser um vetor contendo os numeros das linhas que permanecerao na
#'  imagem (It must be a vector containing the numbers of the lines that will
#'  remain in the image ).
#'@param segmentation matrix binaria obtida por uma segmentacao
#'(Binary matrix obtained of a segmentation)
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
#'@importFrom stats binomial glm predict dist aggregate
#'@importFrom grDevices dev.off  jpeg colorRampPalette rgb
#'@importFrom graphics  lines locator
#'@importFrom utils setTxtProgressBar txtProgressBar unstack
#'@importFrom raster raster
#'@export
#'@examples
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

#'




crop_image=function(im,w=NULL,h=NULL,segmentation=NULL,plot=TRUE,verbose=FALSE){
n=1
  nr=dim(im)[1]
  nc=dim(im)[2]
  l=length(dim(im))
  if(is.null(segmentation)){
  if(!is.null(w)|!is.null(h)){
    if(is.null(w)){w=1:nr}
    if(is.null(h)){h=1:nc}

    if((mode(w)!="numeric")|is.null(h)) {stop("Vectors must be numeric.")}
    if((mode(h)!="numeric")|is.null(w)) {stop("Vectors must be numeric.")}
      if(l==3){im@.Data=im@.Data[w,h,]}
      if(l==2){im=im[w,h]}
   }

   if(is.null(w)&is.null(h)){
     print("Clique em dois vertices opostos para cortar (Click on two opposite vertices to crop)")
     if(EBImage::is.Image(im)){plot_image(im)}
     if(is.matrix(im)){plot_image(EBImage::as.Image((im)))}
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

       if(l==3){im@.Data=im@.Data[w,h,]}
       if(l==2){im=im[w,h]}
     }
}


if(!is.null(segmentation)){
    m=segmentation
    r=(1:nrow(m))[(rowSums(m)!=0)]
    w=(min(r)-n):(max(r)+n)

    c=(1:ncol(m))[(colSums(m)!=0)]
    h=(min(c)-n):(max(c)+n)

    if(l==3){im@.Data=im@.Data[w,h,]}
    if(l==2){im=im[w,h]}
    }








  mm=rbind(
  w=c(round(min(w),0),round(max(w),0)),
  h=c(round(min(h),0),round(max(h),0))
  )

  rownames(mm)=c("min","max")

  if(verbose==TRUE){print(mm)}


  if(plot==T){
    if(EBImage::is.Image(im)){plot_image(im)}
    if(is.matrix(im)){plot_image(EBImage::as.Image((im)))}

  }


  return(im)
}







