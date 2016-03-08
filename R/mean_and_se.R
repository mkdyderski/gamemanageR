#' mean.ray
#'
#' \code{mean.ray}  compute estimated value from vector of Rayleigh distribution
#'
#'\usage{mean.ray(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export

mean.ray<-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),getRayParam(x[which(x>0)])$MR[1]*10*sum(area[which(x>0)])/sum(area))}#rayleigh


mean.gam<-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),
                                 egammaAlt(x[which(x>0)])$parameters[1]*10*sum(area[which(x>0)])/sum(area))}

mean.nor<-function(x,area){ifelse(length(x[x>0])<1,0,mean(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))}
mean.lnor <-function(x,area){ifelse(length(x[x>0])<2,weighted.mean(x,area),
                                   exp(log(mean(x[which(x>0)]))-.5*log(((sd(x[which(x>0)])/mean(x[which(x>0)]))^2)+1))*10*sum(area[which(x>0)])/sum(area))}#lnorm
mean.rgeom<- function(x,area){ifelse(length(x[x>0])<1,0, 1/fitdistr(x[which(x>0)], 'geometric')$estimate[1]*10*sum(area[which(x>0)])/sum(area))}#geom
meangeo<-function(x,area){ifelse(length(x[x>0])<1,0,geoMean(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))}#średnia geonetyrczba

se.ray<-function(x,area) {ifelse(length(x[x>0])<2,0,(getRayParam(x[which(x>0)])$RSD[1]*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh
se.gam<-function(x,area) {ifelse(length(x[x>0])<2,0,(egammaAlt(x[which(x>0)])$parameters[1]*egammaAlt(x[which(x>0)])$parameters[2]*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh
se.nor<-function(x,area) {ifelse(length(x[x>0])<2,0,(sd(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh
se.lnor<-function(x,area) {ifelse(length(x[x>0])<2,0,(exp(sqrt(log(((sd(x[which(x>0)])/mean(x[which(x>0)]))^2)+1)))*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh
se.rgeom<-function(x,area) {ifelse(length(x[x>0])<2,0,(sqrt((1-fitdistr(x[which(x>0)], 'geometric')$estimate[1])/(fitdistr(x[which(x>0)], 'geometric')$estimate[1]^2))*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh
se.geo<-function(x,area) {ifelse(length(x[x>0])<2,0,(geoSD(x[which(x>0)])*10*sum(area[which(x>0)])/sum(area))/sqrt(length(x[which(x>0)])))}#rayleigh#śr gfeom
