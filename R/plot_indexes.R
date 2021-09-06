#' Function to plot multiple indices (funcao para plotar varios indices)
#'
#' @description Function to plot multiple indices (funcao para plotar varios
#'  indices).
#' @usage plot_indexes(im)
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @seealso  \code{\link{gray_scale}}
#' @importFrom graphics par

#' @examples
#' \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'plot_indexes(im)
#'}

#'@export


plot_indexes=function(im){
  #Separar a imagem em bandas


  op <- par(mfrow = c(5,5))
  on.exit(par(op))
  gs=gray_scale(im,method = "r",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"r",col="red",cex=2)
  gs=gray_scale(im,method = "g",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"g",col="red",cex=2)
  gs=gray_scale(im,method = "b",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"b",col="red",cex=2)
  gs=gray_scale(im,method = "rg",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"rg",col="red",cex=2)
  gs=gray_scale(im,method = "rb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"rb",col="red",cex=2)
  gs=gray_scale(im,method = "gb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"gb",col="red",cex=2)
  gs=gray_scale(im,method = "rgb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"rgb",col="red",cex=2)
  gs=gray_scale(im,method = "r/rgb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"r/rgb",col="red",cex=2)
  gs=gray_scale(im,method = "g/rgb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"g/rgb",col="red",cex=2)
  gs=gray_scale(im,method = "b/rgb",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"b/rgb",col="red",cex=2)
  gs=gray_scale(im,method = "BI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"BI",col="red",cex=2)
  gs=gray_scale(im,method = "BIM",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"BIM",col="red",cex=2)
  gs=gray_scale(im,method = "SCI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"SCI",col="red",cex=2)
  gs=gray_scale(im,method = "GLI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"GLI",col="red",cex=2)
  gs=gray_scale(im,method = "HI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"HI",col="red",cex=2)
  gs=gray_scale(im,method = "NGRDI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"NGRDI",col="red",cex=2)
  gs=gray_scale(im,method = "SI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"SI",col="red",cex=2)
  gs=gray_scale(im,method = "VARI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"VARI",col="red",cex=2)
  gs=gray_scale(im,method = "HUE",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"HUE",col="red",cex=2)
  gs=gray_scale(im,method = "MGVRI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"MGVRI",col="red",cex=2)
  gs=gray_scale(im,method = "GLI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"GLI",col="red",cex=2)
  gs=gray_scale(im,method = "MPRI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"MPRI",col="red",cex=2)
  gs=gray_scale(im,method = "RGVBI",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"RGVBI",col="red",cex=2)
  gs=gray_scale(im,method = "ExG",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"ExG",col="red",cex=2)
  gs=gray_scale(im,method = "VEG",plot=TRUE)
  text(nrow(gs)/2,ncol(gs)/2,"VEG",col="red",cex=2)

#dev.off()
}

