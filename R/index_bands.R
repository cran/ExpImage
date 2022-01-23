#' Estimation of vegetation indices (Estimacao de indices de vegetacao).
#'
#' @description This function create vegetation indices
#' (Esta funcao cria indices de vegetacao).
#' @usage index_bands(index="NDVI",red=NULL,nir=NULL,normalize=TRUE)

#' @param index   :Vegetation index to be estimated, default="NDVI" (Indice de vegetacao a ser estimado, default="NDVI").
#' @param red    :Matrix with the red band (Matriz com a banda de vermelho).
#' @param nir    :Matrix with the nir band (Matriz com a banda nir).
#' @param normalize    :Logical value, if TRUE, the result will be normalized to vary between 0 and 1
#'  (Valor logico, se for TRUE o resultado sera normatizado para variar entre 0 e 1).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{gray_scale}} , \code{\link{read_image}}
#

#' @export
index_bands=function(index="NDVI",red=NULL,nir=NULL,normalize=TRUE){

if(index=="NDVI"){
if(is.null(nir)|is.null(red)){
  stop("nir and/or red is NULL")
}
Index=as.matrix((nir-red)/(nir+red))
}

  if(isTRUE(normalize)){Index=Normatiza(Index,Metodo = 2)}
return(Index)

}
