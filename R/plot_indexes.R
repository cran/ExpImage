#' Function to plot multiple indices (funcao para plotar varios indices)
#'
#' @description Function to plot multiple indices (funcao para plotar varios
#'  indices).
#' @usage plot_indexes(im,NumberCores="all")
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param NumberCores Indica o numero de cores a serem utilizados no processamento.
#'   Pode ser um valor numerico. Se for 'ALL' sera considerado o numero maximo de
#'    cores do PC. (Indicates the number of colors to be used in processing.
#'    It can be a numerical value. If it is 'ALL' it will be considered the
#'    maximum number of PC cores).
#' @seealso  \code{\link{gray_scale}}
#' @importFrom graphics par
#' @importFrom parallel clusterExport stopCluster parLapply

#' @examples
#' \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'plot_indexes(im,NumberCores=2)
#'}

#'@export


plot_indexes=function(im,NumberCores="all"){
  #Separar a imagem em bandas
im=EBImage::resize(im,w =200 )

lin=nrow(im@.Data[,,1])
col=ncol(im@.Data[,,1])
lay=length(im@.Data[1,1,])


method=c("r","g","b","rg","rb","gb","rgb","r/rgb","g/rgb","b/rgb",
         "BI","BIM","SCI","GLI","HI",
         "NGRDI","SI","VARI","HUE","MGVRI","GLI","MPRI","RGVBI","ExG","VEG")

if(NumberCores=="all"){NumberCores=detectCores()}

if (NumberCores > detectCores()) {
  message(paste0(" O numero de cores maximo (Maximum number of cores): ", detectCores()))
  NumberCores = detectCores()
}

cl <- parallel::makeCluster(NumberCores)
clusterExport(cl,
              varlist = c("gray_scale","im","method"),
              envir=environment())
on.exit(stopCluster(cl))


r <- parLapply(cl = cl,1:25, function(x){gray_scale(im,method =method[x],plot = F)})


op <- par(mfrow = c(5,5))
#on.exit(par(op))
for(i in 1:25){
  plot_image((r[i][[1]]))
  text(lin/2,col/2,method[i],col="red",cex=2)
  #print(method[i])
}
par(mfrow = c(1,1))
#dev.off()
}










