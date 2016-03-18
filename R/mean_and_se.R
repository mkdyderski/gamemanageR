#' mean.ray
#'
#' \code{mean.ray}  compute estimated value from vector of Rayleigh distribution
#'
#'\usage{mean.ray(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ estimated value computed from vector with Rayleigh distribution using \code{getRayParam} from \code{shotGroups} package}}
#' @details Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
#' @export

mean.ray<-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),shotGroups:::getRayParam(x[which(x>0)])$MR[1]*10*sum(area[which(x>0)])/sum(area))}#rayleigh

#' mean.gam
#'
#' \code{mean.gam}  compute estimated value from vector of Gamma distribution
#'
#'\usage{mean.gam(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ estimated value computed from vector with Gamma distribution using \code{eGammaAlt} from \code{EnvStats} package}}
#' @details Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
#' @export
mean.gam<-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),
                                 EnvStats:::egammaAlt(x[which(x>0)])$parameters[1]*10*sum(area[which(x>0)])/sum(area))}
#' mean.nor
#'
#' \code{mean.nor}  compute estimated value from vector of normal distribution
#'
#'\usage{mean.nor(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ estimated value computed from vector with normal distribution using \code{mean} from \code{base} package}}
#' @details Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
#' @export
mean.nor<-function(x,area){ifelse(length(x[x>0])<1,0,mean(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))}

#' mean.lnor
#'
#' \code{mean.lnor}  compute estimated value from vector of lognormal distribution
#'
#'\usage{mean.lnor(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ estimated value computed from vector with lognormal distribution by... TBA}}
#' @details Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
#' @export

mean.lnor <-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),
                                   exp(log(mean(x[which(x>0)]))-.5*log(((sd(x[which(x>0)])/mean(x[which(x>0)]))^2)+1))*10*sum(area[which(x>0)])/sum(area))}#lnorm

#' mean.rgeom
#'
#' \code{mean.rgeom}  compute estimated value from vector of Geometric distribution
#'
#'\usage{mean.rgeom(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ estimated value computed from vector with Geometric distribution using \code{fitdistr} from \code{MASS} package}}
#' @details Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
#' @export
#'
mean.rgeom<- function(x,area){ifelse(length(x[x>0])<1,0, 1/MASS:::fitdistr(x[which(x>0)], 'geometric')$estimate[1]*10*sum(area[which(x>0)])/sum(area))}#geom

#' meangeo
#'
#' \code{meangeo}  compute geometric mean
#'
#'\usage{meangeo(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ geometric mean computed using \code{geoMean} from \code{EnvStats} package}}
#' @details Here will be described how it works TBA
#' @export
#'
meangeo<-function(x,area){ifelse(length(x[x>0])<1,0,EnvStats:::geoMean(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))}#średnia geonetyrczba

#' se.ray
#'
#' \code{se.ray}  compute standard error from vector of Rayleigh distribution
#'
#'\usage{se.ray(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations, sd computed using \code{getRayParam} from \code{shotGroups} package}}
#' @details Here will be described how it works TBA
#' @export
#'
se.ray<-function(x,area) {ifelse(length(x[x>0])<2,0,(shotGroups:::getRayParam(x[which(x>0)])$RSD[1]*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh

#' se.gam
#'
#' \code{se.gam}  compute standard error from vector of Gamma distribution
#'
#'\usage{se.gam(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations, sd computed using \code{eGammaAlt} from \code{EnvStats} package}}
#' @details Here will be described how it works TBA
#' @export
#'
se.gam<-function(x,area) {ifelse(length(x[x>0])<2,0,( EnvStats:::egammaAlt(x[which(x>0)])$parameters[1]* EnvStats:::egammaAlt(x[which(x>0)])$parameters[2]*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh

#' se.nor
#'
#' \code{se.nor}  compute standard error from vector of normal distribution
#'
#'\usage{se.nor(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations}}
#' @details Here will be described how it works TBA
#' @export
#'
se.nor<-function(x,area) {ifelse(length(x[x>0])<2,0,(sd(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh

#' se.lnor
#'
#' \code{se.lnor}  compute standard error from vector of lognormal distribution
#'
#'\usage{se.lnor(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations, sd computed as... TBA}}
#' @details Here will be described how it works TBA
#' @export
#'
se.lnor<-function(x,area) {ifelse(length(x[x>0])<2,0,(exp(sqrt(log(((sd(x[which(x>0)])/mean(x[which(x>0)]))^2)+1)))*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh

#' se.rgeom
#'
#' \code{se.rgeom}  compute standard error from vector of geometric distribution
#'
#'\usage{se.rgeom(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations, sd computed using \code{fitdistr} from \code{MASS} package}}
#' @details Here will be described how it works TBA
#' @export
#'
se.rgeom<-function(x,area) {ifelse(length(x[x>0])<2,0,(sqrt((1-MASS:::fitdistr(x[which(x>0)], 'geometric')$estimate[1])/(MASS:::fitdistr(x[which(x>0)], 'geometric')$estimate[1]^2))*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh

#' se.geo
#'
#' \code{se.geo}  compute standard error off geometric mean
#'
#'\usage{se.geo(x, area)}
#' @param x a vector containing observations, e.g. game densities
#' @param area a vector containing areas of each drives in ha
#' @return
#'\describe{
#'\item{value}{ standard error computed as \code{sd/sqrt(n)} of non-zero observations, sd computed using \code{geoSD} from \code{EnvStats} package}}
#' @details Here will be described how it works TBA
#' @export
#'
se.geo<-function(x,area) {ifelse(length(x[x>0])<2,0,(EnvStats:::geoSD(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh#śr gfeom
