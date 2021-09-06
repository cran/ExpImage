#' Images used as an example (Banco de imagens utilizadas como exemplo).
#'
#' @description Show the path of images used in the example file (Apresenta
#'   endereco de imagens utilizadas no arquivo de exemplo).
#' @usage example_image(ex)

#' @param ex    :example number (numero do exemplo).
#' The numbers corresponding to each example are(Os numeros correspondentes a
#' cada exemplo sao):
#' \itemize{
#'  \item 1 = Imagem com sementes de feijao fava
#'  \item 2 = Imagem com ovos sobre folha de fumo
#'  \item 3 = Imagem com folhas de acereola
#'  \item 4 = Imagem com a paleta de cores do background da imagem com folhas de
#'  acerola
#'  \item 5 = Imagem com a paleta de cores das folhas de acerola
#'  \item 6 = Imagem com a paleta de cores do obejeto de referencia da imagem
#'  com folhas de  acerola
#'  \item 7 = Imagem com folha de tomate com doenca
#'  \item 8 = Imagem com paleta de cores das partes sadias da folha de tomateiro
#'  \item 9 = Imagem com a paleta de cores das partes doentes da folha de
#'  tomateiro
#'  \item 10 = Imagem de uma plantula
#'  \item 11 = Imagem aerea de um rebanho (https://www.istockphoto.com/)
#'  }

#' @return Returns the address of the example images (Retorna o endereco das
#'   imagens de exemplo).

#' @examples
# \donttest{
#' example_image(1)
#' example_image(2)
#}
#'@export
example_image=function(ex) {
  if((ex<1)|(ex>12)){stop("Image not available for this number", call. = FALSE)}
  Imagens=c("Feijao.jpg","Ovos1.jpg","imagem.jpg","fundo.jpg","folhas.jpg","Referencia.jpg",
            "FolhaTomate.jpg","TomateFolha.jpg","TomateDoenca.jpg","Plantula.jpg","gado.jpg","alface.jpg")

  #system.file(paste0("images/", Imagens[ex]), package = "ExpImage")
  system.file("images",Imagens[ex],package="ExpImage")
}





