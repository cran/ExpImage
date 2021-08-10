# @importFrom base packageStartupMessage paste0
#' @importFrom crayon green bold
# --------------------------------------
# on loading

.onAttach = function(library, pkg){
if(interactive()) {
  #vers <-  read.dcf(file.path(libname, pkgname, "DESCRIPTION"),appendLF=TRUE), "Version")
  packageStartupMessage(green("############################################################"),appendLF=TRUE)
  packageStartupMessage(green(paste0("Obrigado por utilizar o ", bold("ExpImage") )),appendLF=TRUE)#, vers)
  packageStartupMessage(green("Author: Alcinei Mistico Azevedo (ICA-UFMG)"),appendLF=TRUE)
  packageStartupMessage(green("Veja tutoriais sobre este e outros pacotes no youtube:"),appendLF=TRUE)
  packageStartupMessage(green("https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ"),appendLF=TRUE)
  packageStartupMessage(green("Se inscreva e compartilhe para ajudar o canal a crescer."),appendLF=TRUE)
  packageStartupMessage(green("############################################################"),appendLF=TRUE)
}
  invisible()
}
