#' game.load.pl
#'
#' \code{game.load.pl}  loads .csv data from long table
#'
#'
#'\usage{game.load.pl(x, sep=';')}
#' @param x .csv file exported from Excel in long format, see details below
#' @param sep separator of csv file, deflaut is ';'
#' @return Data.frame without zero-obs species [in case of] densities within each plot
#' @details To use file there is needed a specific file structure, with information about number of trail, trail area, and then - counts of each species. TBA

#' @export
game.load.pl<-function(x,sep=';'){
  #----processing function used to prepare data from long tables
    n1<-read.csv(x,sep=sep)[,-52]
    nkoli<-c('miot', 'oddzialy','tsl','pow','nr_obs',
             'j_byk','j_lania','j_ciele','j_nn','jelen',
             's_koziol','s_koza','s_kozle','s_nn','sarna',
             'd_starsze','d_przelatek','d_warchlak','d_nn','dzik',
             'da_byk','da_lania','da_ciele','da_nn','daniel',
             'los_byk','los_klempa','los_loszak','los_nn','los',
             'z_byk','z_krowa','z_ciele','z_nn','zubr',
             'm_tryk','m_owca','m_jagnie','m_nn','muflon',
             'si_byk','si_lania','si_ciele','si_nn','jelen_sika',
             'zajac','wilk','lis','borsuk','jenot','inne')
    colnames(n1)<-nkoli
    wiersze.miot<-which(n1$nr_obs=='razem')
    df.rob<-n1[wiersze.miot,c(1,2,3,4,5,10,15,20,25,30,35,40)]
    #wypierdalamy gatunki, które są zerem
    df.rob<-df.rob[,-(which(colSums(df.rob[,-c(1:5)])==0)+5)]
    df.rob<-df.rob[-c(2,3,5)]
    for (i in 3:ncol(df.rob)) df.rob[,i]<-(df.rob[,i]/df.rob$pow)*100
  df.rob}

#' game.load
#'
#' \code{game.load}  loads .csv data from long table
#'
#'
#'\usage{game.load(x, sep=';')}
#' @param x .csv file exported from Excel in short format, see details below
#' @param sep separator of csv file, deflaut is ';'
#' @return Data.frame with species densities within each plot
#' @details To use file there is needed a file with an ordered structure - drive number, drive area and then - species in each column TBA

#' @export
game.load<-function(x,sep){
  df<-read.csv(x, sep=sep)
  for (i in 3:ncol(df)) df[,i]<-(df[,i]/df[,2])*100
  df}


