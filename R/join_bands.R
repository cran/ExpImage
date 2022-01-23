#'Function to compare image sizes  (Funcao para comparar os tamanhos das imagens).
#'
#' @description This function compares the size of images  (Esta funcao compara o tamanho das imagens).
#' @usage join_bands(imgs=NULL,filesnames=NULL,path = NULL)

#' @param imgs    :List object containing the images  (Objeto do tipo lista contendo as imagens).
#' @param filesnames    :Images names (Nomes das imagens).
#' @param path    :Path files  (Endereco das pastas).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#' #Carregando imagens de exemplo
#' im1=read_image(example_image(2),plot=TRUE)
#' r=gray_scale(im = im1,method = "r")
#' g=gray_scale(im = im1,method = "g")
#' b=gray_scale(im = im1,method = "b")
#'
#' im2=join_bands(imgs = list(r,g,b))
#' plot_image(im2)
#
#'@export
#'

join_bands=function(imgs=NULL,filesnames=NULL,path = NULL){

  if(!is.null(path)){ filesnames=paste(path,filesnames,sep="/")}





  if(is.null(imgs)&!is.null(filesnames)){
  imgs=list()

  pb <- progress::progress_bar$new(total = length(filesnames))
    for(i in 1:length(filesnames)){
      pb$tick()
      imgs[[i]]=read_image(filesnames[i])
    }

  }


RES=NULL

a=imgs[[1]]
if(!EBImage::is.Image(a)) {a=as_image(a)}
arr=array(NA,c(dim(imgs[[1]]),length(imgs)))
arr[,,1]=a@.Data

#pb1 <- progress::progress_bar$new(total = length(filesnames))
for(i in 2:length(imgs)){
 # pb1$tick()

  arr[,,i]=imgs[[i]]@.Data
  }

a@.Data=arr
a@colormode=length(imgs)

return(a)
}
