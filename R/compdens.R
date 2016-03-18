#' compdens
#'
#' \code{compdens}  compute mean and SE for denisties using six methods, and p-values of ks tests
#'
#'\usage{compdens(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{data frame with mean, SE and results of ks tests for each method TBA}}
#' @details Here will be described how it works TBA
#' @export
#'
compdens<-function(x, area){
    srednie<-data.frame(distribution=c('Rayleigh', 'Gamma', 'Normal','Lognormal','Geometric','Geometric mean'),
                      mean=c(mean.ray(x, area),#rayleigh
                             mean.gam(x, area),#gamma
                            mean.nor(x, area),#norm
                             mean.lnor(x, area),#lnorm
                             mean.rgeom(x, area),#geom
                             meangeo(x, area)),#end of mean section
                      se=c(se.ray(x, area),#rayleigh
                           se.gam(x, area),#gamma
                           se.nor(x, area),#norm
                           se.lnor(x, area),#lnorm
                           se.rgeom(x, area),#geom
                           se.geo(x, area)),
                      distribution=c(ts.ray(x),
                                ts.gam(x),
                                ts.nor(x),
                                ts.lnorm(x),
                                ts.geom(x),NA))
  srednie}



#' comptab
#'
#' \code{comptab}  compute mean and SE for denisties using six methods, and p-values of ks tests
#'
#'\usage{comptab(x)}
#' @param x data frame containing mean, se and p-values of tests, computed by compdens
#' @return
#'\describe{
#'\item{value}{data frame with results for a specified region, sorted by species. TBA}}
#' @details Here will be described how it works TBA
#' @export
#'

comptab<-function(x){
  n<-ncol(x)-2
  listki<-array(NA, c(6,4,n))
  for (i in 1:n) listki[,,i]<-as.matrix(compdens(x[,2+i],x[,2]))
  spec<-matrix(NA, n,4)
  spec[,1]<-'species:'
  spec[,2]<-colnames(x)[3:(n+2)]

  wyn<-matrix(NA, 7*n,4)#7*n
  for (i in 1:n) wyn[i*7-6,] <-spec[i,]
  for (i in 1:n)wyn[((7*(i-1))+2):(7*i),] <-listki[,,i]
  wyn<-as.data.frame(wyn)
  colnames(wyn)<-c('method','mean','se','p')
  wyn}
