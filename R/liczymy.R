#' liczymy
#'
#' \code{liczymy}  function dedicated for quick single-region data obs.
#'
#'\usage{liczymy(x, sep=';', long=TRUE)}
#' @param x .csv file exported from Excel, see details below
#' @param sep separator of csv file, deflaut is ';'
#' @param long logical flag, whether table type is long or short
#' @return
#'\describe{
#'\item{value}{data frame with mean, SE and results of ks tests for each method TBA}}
#' @details Here will be described how it works TBA
#' @export
#'

liczymy<-function(x, sep=';', long=TRUE){
  #load data, depndly on format
    if(long==TRUE)      dfram<-game.load.pl(x, sep=';')      else #jak długa
    dfram<- game.load(x, sep=';')
  #
dfram2<-comptab(dfram)
nazwa<-paste('po_',x, sep='')
write.table(dfram2, as.character(nazwa) , sep = ";", row.names = FALSE, col.names=TRUE)}
