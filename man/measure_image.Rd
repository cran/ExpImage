% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_image.R
\name{measure_image}
\alias{measure_image}
\title{Function to obtain measurements associated with objects in binary images
(Funcao para obter medidas associadas aos objetos em imagens binarias).}
\usage{
measure_image(img,noise=0,id=NULL,length= NULL,width =NULL, splitConnected=FALSE,
tolerance = 1, ext = 1,imOut=FALSE,  plot= TRUE)
}
\arguments{
\item{img}{:Este objeto deve ser obrigatoriamente uma matriz binaria,
contendo os valores 0 (pixels do background) e 1 (pixels do foreground)).}

\item{noise}{: E o numero de pixeis a partir do qual a funcao nao
considerara como ruido.}

\item{id}{:Se igual a NULL (default) nao sera feita a conversao de pixels
para centimetros. Se houver algum objeto de referencia na imagem com area
conhecida, deve-se colocar o numero referente a este objeto. Se o tamanho
da imagem em centimetros for conhecida, pode-se colocar para este argumento
a palavra "all".}

\item{length}{:Comprimento do objeto de referencia ou da imagem em
centimetros.}

\item{width}{:Altura do objeto de referencia ou da imagem em centimetros.}

\item{splitConnected}{:Variavel do tipo logico. Se TRUE objetos encostados
serao considerados diferentes.}

\item{tolerance}{The minimum height of the object in the units of image
intensity between its highest point (seed) and the point where it contacts
another object (checked for every contact pixel). If the height is smaller
than the tolerance, the object will be combined with one of its neighbors,
which is the highest. Tolerance should be chosen according to the range of
x. Default value is 1, which is a reasonable value if x comes from distmap.}

\item{ext}{Radius of the neighborhood in pixels for the detection of
neighboring objects. Higher value smoothes out small objects.}

\item{imOut}{Logical variable, if TRUE, the segmentation image will be exported in the results.
(Variavel logica, se for TRUE sera exportada a imagem da segmentacao nos resultados).}

\item{plot}{Indicates whether the segmented image will be displayed (TRUE) or not (FALSE) (default)
(Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
  imagem segmentada)}
}
\value{
Returns the coordinates of each object, its area, perimeter, ...(Retorna as cordendas de cada objeto, sua area, perimetro, ...)
}
\description{
Esta funcao possibilita a obtencao de medidas associadas aos
  objetos em imagens binarias.
}
\examples{
\donttest{

############################################################################
#Obtendo o numero de ovos em uma folha
############################################################################

#Carregar imagem de exemplo
im=read_image(example_image(2))
##mostrar imagem
plot_image(im)

#Selecionando o melhor indice para a segmentacao da folha
r=gray_scale(im,method = "r",plot=TRUE)
g=gray_scale(im,method = "g",plot=TRUE)
b=gray_scale(im,method = "b",plot=TRUE)


#O canal azul possibilita maior contraste
#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
MatrizSegmentada=segmentation(b,threshold = 0.40,fillHull = TRUE,
selectHigher = FALSE,plot=TRUE)

im2=extract_pixels(im,target =MatrizSegmentada,valueTarget =1,
valueSelect = c(0,0,0),plot=TRUE )


#Selecionando o melhor indice para a segmentacao dos ovos
r=gray_scale(im2,method = "r",plot=TRUE)
g=gray_scale(im2,method = "g",plot=TRUE)
b=gray_scale(im2,method = "b",plot=TRUE)

#O canal Azul proporciona melhor segmentacao
#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
MatrizSegmentada2=segmentation(b,threshold = 0.60,fillHull = TRUE,
selectHigher = TRUE,plot = TRUE)

Medidas=measure_image(MatrizSegmentada2)
Medidas$ObjectNumber

#Ver a mascara sobre os ovos na foto
im3=mask_pixels(im,MatrizSegmentada2==1,plot=TRUE)

#############################################################
#Obtendo a area de folhas de acerola
##############################################################


#ativar pacote
library(ExpImage)
#Abrir imagem
im=read_image(example_image(3))


#Selecionando o melhor indice para a segmentacao
r=gray_scale(im,method ="r",plot=TRUE)
g=gray_scale(im,method = "g",plot=TRUE)
b=gray_scale(im,method ="b",plot=TRUE)


#A banda de azul foi a que melhor discriminou #O limiar pode ser um valor
#escolhido aleatoriamente (por exemplo: 0.6)
MatrizSegmentada=segmentation(b,threshold = 0.6,fillHull = FALSE,
selectHigher =FALSE,plot=TRUE)

#O limiar tambem pode ser estabelecido pelo metodo de otsu
MatrizSegmentada2=segmentation(b,threshold = "otsu",fillHull = TRUE,
selectHigher =FALSE,plot=TRUE)

#Obter medidas de cada objeto
medidas=measure_image(MatrizSegmentada2)
#ver o numero de objetos e medias medidas
medidas$ObjectNumber

#Obter medidas de cada objeto excluindo o ruido
medidas=measure_image(MatrizSegmentada2,noise = 1000) #numero de objetos
medidas$ObjectNumber
Estimativas=medidas$measures

#Plotar resultados das areas em pixel e salvar em um arquivo chamado "teste.jpg"
#plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#text=round(medidas$measures[,3],1),cex= 0.9,pathSave ="teste.jpg",
#col="blue" ,plot = TRUE)


plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
text=round(medidas$measures[,3],2),cex = 0.9,col="blue" ,plot=TRUE)
##############################################################################
#Convertendo a area dos objetos para cm2

#Conhecendo o identificador do objeto de referencia

plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
text=rownames(medidas$measures),cex= 0.9,
col="blue",plot=TRUE )
#como pode-se ver, o objeto de referencia e o de numero 30
# A area conhecida do objeto de referencia tem 8.5 x 5.5 cm.
#Isso nos leva a 46.75
medidas2=measure_image(MatrizSegmentada2,noise = 1000,id=30,
length= 8.5,width =5.5)
medidas2
#Apresentando a area foliar em cm2 de sobre cada uma das folhas
plot_meansures(im,medidas2$measures[,1],coordy=medidas2$measures[,2],
text=round(medidas2$measures[,3],2),cex = 0.9,col="blue")


################################################################
#Obs.: O uso do objeto de referencia e util para a conversao em cm2 em
#situacoes que nao se conhece a area fotografada.
#Se soubermos exatamente qual e o tamanho da area escaneada (fotografada)
#podemos dispensar o uso do objeto de referencia.

#Convertendo a area em pixel para cm2 considerando a dimensao superficie
#escaneada.
# A dimensao da superficie escaneada tem 21*29.7 cm (dimensao de uma folha a4).
#Isso nos leva a  623.7 cm2

medidas3=measure_image(MatrizSegmentada2,noise = 1000,id="all",
length= 21,width =29.7)
medidas3
#Apresentando a area foliar de sobre cada uma das folhas
plot_meansures(im,medidas3$measures[,1],coordy=medidas3$measures[,2],
text=round(medidas3$measures[,3],2),cex = 0.9,col="blue",plot=TRUE)
}
}
\seealso{
\code{\link{segmentation_logit} , \link{segmentation}}
}
