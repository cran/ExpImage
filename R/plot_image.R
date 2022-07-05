#'Esta funcao plota imagens. (This function plot the images.)
#'@description Esta funcao permite plotar a imagem (This function allows you to
#'  view the image).
#' @usage plot_image(im,r=1,g=2,b=3,band=NULL,col=0)
#' @param im Este objeto deve conter uma imagem no formato do EBImage ou na forma
#'de uma matriz, no caso de imagem em escala de cinza (This
#'  object must contain an image in EBImage format).
#'@param r Indica o canal correspondente a cor vermelha para imagens com
#'   extensao '.tif'. O defaut e 1. (Indicates the channel corresponding to
#'   red color for images with the extension '.tif'. The default is 1.)
#'@param g Indica o canal correspondente a cor verde para imagens com
#'   extensao '.tif'. O defaut e 2. (Indicates the channel corresponding to
#'   green color for images with the extension '.tif'. The default is 2.)
#'@param b Indica o canal correspondente a cor azul para imagens com
#'   extensao '.tif'. O defaut e 3. (Indicates the channel corresponding to
#'   blue color for images with the extension '.tif'. The default is 3.)
#'@param band Indica a banda que se deseja plotar. Neste caso nao se precisa
#'considerar as bandas de R, G e B simultaneamente.

#'@param col Pode ser um valor numerico variando entre 0 e 6 ou uma paleta de cores
#'obtida pela funcao `colorRampPalette`. Se for 0 sera considerada a representacao
#'da imagem monocromatica em escala de cinza. Valores entre 1 e 6 indicam outras
#'paletas de cores para a representacao. (It can be a numerical value ranging from
#'0 to 6 or a color palette 'obtained by the `colorRampPalette` function. If it is
#'0, the representation of the monochromatic image in gray scale will be considered.
#' Values between 1 and 6 indicate other color palettes for the representation. )
#'
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Apresenta a imagem contida no objeto im.
#'@seealso  \code{\link{read_image}}
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg recordPlot
#'@importFrom graphics points
#'@importFrom raster plotRGB  stack raster
#'@importFrom parallel detectCores
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach %dopar% foreach
#'@export
#' @examples
#'
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#' im=resize_image(im,w = 300,plot = FALSE)
#'plot_image(im)
#'
#'#Representado imagens em escalas de cinza
#'im2 = gray_scale(im,"r")
#'plot_image(im2)
#'#Utilizando uma paleta de cores para a melhor visualizacao
#'plot_image(im2,col=2)
#'





plot_image=function(im,r=1,g=2,b=3,band=NULL,col=0){
  Rows=Cols=Value=0
if(is.null(class(im)[1])){
  class(im)="aaa"
}

  if(class(im)[1]=="RasterStack"){
    if(is.null(band)){plotRGB(im,r=r,g=g,b=b)}
    if(!is.null(band)){im=EBImage::as.Image(im@.Data[,,band])}
    }

  if(class(im)[1]!="RasterStack"){
    if(!is.null(band)){im=EBImage::as.Image(im@.Data[,,band])
    im@colormode=as.integer(0)}

  if(EBImage::is.Image(im)|is.matrix(im)){
    #if(EBImage::is.Image(im)){im=im@.Data}


   if(is.matrix(im)) {im=EBImage::as.Image(im)}

    if( (im@colormode)>0){
      im2=EBImage::as.Image(im@.Data[,,c(r,g,b)])
      im2@colormode<-as.integer(2)

        plot(im2)

    }

    if((im@colormode)==0){
      im2=EBImage::as.Image(im)
      if(col==0){
        plot(im2)
      }
      if(col>0){
im=EBImage::as.Image(im)
im=EBImage::flip(im)
im=as.matrix(im@.Data)
        paleta=colorRamp_Palette(col)

imm=linearize_image(im)



        IM=data.frame(Rows=imm[,1],Cols=imm[,2],Value=imm[,3])
        ggplot2::ggplot( data=IM,
                         ggplot2::aes( x=Rows, y=Cols, fill=Value)) +
          ggplot2::geom_raster(interpolate= TRUE)  +
          ggplot2::scale_fill_gradientn(colours = paleta(100))    +
          ggplot2::theme_void()
      }
    }

  }
  }
}




colorRamp_Palette=function(m=1){
  if(m==1){ col = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))}

  if(m==2){col = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
                                    'cyan', '#007FFF', 'blue','#00007F'))}


  if(m==3){col= colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                                   '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                                   '#4393C3', '#2166AC', '#053061'))}

  if(m==4){col = colorRampPalette(c('red', 'white', 'blue')) }

  if(m==5){col = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
                                    'cyan', '#007FFF', 'blue', '#00007F')) }

  if(m==6){col = colorRampPalette(c("#669900","yellow","#FF5500")) }

  return(col)

}

