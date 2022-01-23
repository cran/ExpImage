#' Create a histogram from image bands
#' (Criar um histrograma a partir das bandas das imagens)
#'
#' @description This function allows you to create histograms from the bands of the images (Esta funcao permite criar histogramas a partir das bandas das imagens).
#' @usage histogram_image(im,layout=1)

#' @param im    :Este objeto deve conter uma imagem (This object must contain an image ).
#' @param layout    : Valor numerico variando entre 1 e 3 para se ter diferentes layouts
#' (Numeric value ranging between 1 and 3 to have different layouts).

#' @return Retorna histogramas a partir das bandas (Return histograms from the bands of the images).
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
#\donttest{
#' end=example_image(6)
#' im=read_image(end,plot=TRUE)
#' histogram_image(im,layout = 1)
#' histogram_image(im,layout = 2)
#' histogram_image(im,layout = 3)
#}

#' @export
histogram_image=function(im,layout=1){

  Bands=Rows=Cols=Value=0

if(is.matrix(im)){
  im=EBImage::as.Image(im)
}

  if(length(dim(im))==2){
    arr=array(NA,dim = c(dim(im),1))
    arr[,,1]=im@.Data
  }

if(length(dim(im))==3){
  arr=im@.Data
}

if(length(dim(im))>3){
  stop("The file must be a image or matrix type")
}

  DIM=dim(arr)


  arr2=NULL
  for(i in 1:DIM[3]){
    arr2=rbind(arr2,cbind(paste("Band",i),linearize_image(arr[,,i])))
  }

  colnames(arr2)=c("Bands","Row","Col","Value")
  arr2$Band=as.factor( arr2$Band)

  if(layout==1){
  PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,color=Bands))+
    ggplot2::geom_histogram(alpha=0.2,position="identity",bins = 30)+
    ggplot2::theme_classic()
  PLOT
  }

  if(layout==2){
    PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,fill=Bands))+
      ggplot2::geom_histogram(color="black",bins = 30)+
      ggplot2::facet_grid(Bands~.)+
      ggplot2::theme_classic()
    PLOT
  }

  if(layout==3){
    PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,fill=Bands))+
      ggplot2::geom_histogram(colour="black",bins = 30)+
      ggplot2::facet_wrap(Bands~.)+
      ggplot2::theme_classic()
    PLOT
  }
return(PLOT)
}
