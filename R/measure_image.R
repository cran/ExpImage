#' Function to obtain measurements associated with objects in binary images
#' (Funcao para obter medidas associadas aos objetos em imagens binarias).
#'
#' @description Esta funcao possibilita a obtencao de medidas associadas aos
#'   objetos em imagens binarias.
#' @usage measure_image(img,noise=0,id=NULL,length= NULL,width =NULL)
#' @param img    :Este objeto deve ser obrigatoriamente uma matriz binaria,
#'   contendo os valores 0 (pixels do background) e 1 (pixels do foreground)).
#' @param noise    : E o numero de pixeis a partir do qual a funcao nao
#'   considerara como ruido.
#' @param id    :Se igual a NULL (default) nao sera feita a conversao de pixels
#'   para centimetros. Se houver algum objeto de referencia na imagem com area
#'   conhecida, deve-se colocar o numero referente a este objeto. Se o tamanho
#'   da imagem em centimetros for conhecida, pode-se colocar para este argumento
#'   a palavra "all".
#' @param length    :Comprimento do objeto de referencia ou da imagem em
#'   centimetros.
#' @param width    :Altura do objeto de referencia ou da imagem em centimetros.

#' @return Retorna as cordendas de cada objeto, sua area, perimetro, ...
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
#'\donttest{
#'
#'############################################################################
#'#Obtendo o numero de ovos em uma folha
#'############################################################################
#'
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'##mostrar imagem
#'plot(im)

#'
#'#Selecionando o melhor indice para a segmentacao da folha
#'r=gray_scale(im,method = "r",plot=TRUE)
#'g=gray_scale(im,method = "g",plot=TRUE)
#'b=gray_scale(im,method = "b",plot=TRUE)
#'
#'
#'#O canal azul possibilita maior contraste
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada=segmentation(b,treshold = 0.30,fillHull = TRUE,
#'selectHigher = FALSE,plot=TRUE)
#'
#'im2=extract_pixels(im,target =MatrizSegmentada,valueTarget =1,
#'valueSelect = c(0,0,0),plot=TRUE )
#'
#'
#'#Selecionando o melhor indice para a segmentacao dos ovos
#'r=gray_scale(im2,method = "r",plot=TRUE)
#'g=gray_scale(im2,method = "g",plot=TRUE)
#'b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'#O canal Azul proporciona melhor segmentacao
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada2=segmentation(b,treshold = 0.50,fillHull = TRUE,
#'selectHigher = TRUE,plot = TRUE)
#'
#'Medidas=measure_image(MatrizSegmentada2)
#'Medidas$ObjectNumber
#'
#'#Ver a mascara sobre os ovos na foto
#'im3=mask_pixels(im,MatrizSegmentada2==1,plot=TRUE)


#'
#'#############################################################
#' #Obtendo a area de folhas de acerola
#'##############################################################
#'
#'
#'#ativar pacote
#' library(EBImage)
#' library(ExpImage)
#' #Abrir imagem
#'im=read_image(example_image(3))
#'
#'
#'#Selecionando o melhor indice para a segmentacao
#' r=gray_scale(im,method ="r",plot=TRUE)
#' g=gray_scale(im,method = "g",plot=TRUE)
#' b=gray_scale(im,method ="b",plot=TRUE)
#'
#'
#'#A banda de azul foi a que melhor discriminou #O limiar pode ser um valor
#'#escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada=segmentation(b,treshold = 0.6,fillHull = FALSE,
#'selectHigher =FALSE,plot=TRUE)
#'
#'#O limiar tambem pode ser estabelecido pelo metodo de otsu
#'MatrizSegmentada2=segmentation(b,treshold = "otsu",fillHull = TRUE,
#'selectHigher =FALSE,plot=TRUE)
#'
#'#Obter medidas de cada objeto
#'medidas=measure_image(MatrizSegmentada2)
#'#ver o numero de objetos e medias medidas
#'
#'
#'#Obter medidas de cada objeto excluindo o ruido
#'medidas=measure_image(MatrizSegmentada2,noise = 1000) #numero de objetos
#'medidas$ObjectNumber
#'Estimativas=medidas$measures
#'
#'#Plotar resultados das areas em pixel e salvar em um arquivo chamado "teste.jpg"
#'#plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'#text=round(medidas$measures[,3],1),cex= 0.9,pathSave ="teste.jpg",
#'#col="blue" ,plot = TRUE)
#'
#'
#'#plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'#text=round(medidas$measures[,11],2),cex = 0.9,pathSave ="teste.jpg",
#'#col="blue" ,plot=TRUE)
#'##############################################################################
#'#Convertendo a area dos objetos para cm2
#'
#'#Conhecendo o identificador do objeto de referencia
#'
#'#plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'#text=rownames(medidas$measures),cex= 0.9,pathSave ="teste.jpg",
#'#col="blue",plot=TRUE )

#'#como pode-se ver, o objeto de referencia e o de numero 30
#'# A area conhecida do objeto de referencia tem 8.5 x 5.5 cm.
#'#Isso nos leva a 46.75
#'medidas2=measure_image(MatrizSegmentada2,noise = 1000,id=30,
#'length= 8.5,width =5.5)
#'medidas2
#'#Apresentando a area foliar em cm2 de sobre cada uma das folhas
#'plot_meansures(im,medidas2$measures[,1],coordy=medidas2$measures[,2],
#'text=round(medidas2$measures[,3],2),cex = 0.9,col="blue")
#'
#'
#'################################################################
#'#Obs.: O uso do objeto de referencia e util para a conversao em cm2 em
#'#situacoes que nao se conhece a area fotografada.
#'#Se soubermos exatamente qual e o tamanho da area escaneada (fotografada)
#'#podemos dispensar o uso do objeto de referencia.
#'
#'#Convertendo a area em pixel para cm2 considerando a dimensao superficie
#' #escaneada.
#'# A dimensao da superficie escaneada tem 21*29.7 cm (dimensao de uma folha a4).
#'#Isso nos leva a  623.7 cm2
#'
#'medidas3=measure_image(MatrizSegmentada2,noise = 1000,id="all",
#'length= 21,width =29.7)
#'medidas3
#'#Apresentando a area foliar de sobre cada uma das folhas
#'plot_meansures(im,medidas3$measures[,1],coordy=medidas3$measures[,2],
#'text=round(medidas3$measures[,3],2),cex = 0.9,col="blue",plot=TRUE)
#'}
#'@export


measure_image=function(img,noise=0,id=NULL,length= NULL,width =NULL){
  MatrizSegentada2=img
  MatrizSegentada3=bwlabel(MatrizSegentada2)
  res=computeFeatures.shape(MatrizSegentada3)
  ID=res[,1]>noise






  if(nrow(res)>1){
    #Exluir os ruidos

    res2=res[ID,]

    coord=computeFeatures.moment(MatrizSegentada3)
    coord=coord[ID,]

    RES=cbind(coord[,1:2],res2,coord[,3:5])

    rownames(RES)=1:nrow(RES)
  }


  if(nrow(res)==1){
    #Exluir os ruidos
    res2=res

    coord=computeFeatures.moment(MatrizSegentada3)
    coord=coord

    RES=c(coord[1:2],res2,coord[3:5])
    names(RES)=c( "m.cx"    ,  "m.cy" ,"s.area" ,"s.perimeter", "s.radius.mean"
                  ,"s.radius.sd", "s.radius.min", "s.radius.max")

  }

  RES2=RES

  if(is.matrix(id)){
    id2=id
    Area=length*width
    RES=RES2
    RES[,3]= RES[,3]*Area/sum(id2)

    perim=bwlabel(id)
    perim2=computeFeatures.shape(perim)

    if(nrow(perim2)>1){
      perim3=perim2[perim[,1]==max(perim[,1]),2]
    }

    if(nrow(perim2)==1){
      perim3=perim2[2]
    }

    RES[,4:9]= RES[,4:9]*((length+width)*2 )/ perim3


  }
  if(is.Image(id)){
    id2=id@.Data
    Area=length*width
    RES=RES2
    RES[,3]= RES[,3]*Area/sum(id2)

    perim=bwlabel(perim)
    perim=computeFeatures.shape(perim)
    perim2=perim[perim[,1]==max(perim[,1]),4]

    RES[,4:9]= RES[,4:9]*((length+width)*2 )/ perim2


  }


  if((is.matrix(id)+is.Image(id))==0){

    if(isFALSE(is.null(id))){

      if(id!="all"){
        # length= 8.5
        #width =5.5
        Area=length*width

        RES[,3]= RES[,3]*Area/RES[id,3]



        RES[,4:9]= RES[,4:9]*((length+width)*2 )/ RES[id,4]
      }


      if(id=="all"){

 Area=length*width
   RES[,3]= RES[,3]*Area/(nrow(MatrizSegentada2)*ncol(MatrizSegentada2))
  # RES[id,4:9]* (nrow(MatrizSegentada2)*ncol(MatrizSegentada2))/ ((length+width)*2 )
RES[,4:9]= RES[,4:9]*((length+width)*2 )/ ((nrow(MatrizSegentada2)+ncol(MatrizSegentada2)) *2)


      }

    }
  }


  #Verificando numero de objetos
  ObjectNumber=nrow(res2)

  return(list(ObjectNumber=ObjectNumber,measures=RES))

}
