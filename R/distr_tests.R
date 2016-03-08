#' ts.ray
#'
#' \code{ts.ray}  tests empiric distribution versus Rayleigh theoretical distribution
#'
#'
#'\usage{ts.ray(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export
ts.ray<-function(x) {ifelse(length(x[x>0])<2,0, ks.test(x[which(x>0)], VGAM:::rrayleigh(1000, shotGroups:::getRayParam(x[which(x>0)])$sigma[1]))$p.value)}

#' ts.gam
#'
#' \code{ts.gam}  tests empiric distribution versus Gamma theoretical distribution
#'
#'
#'\usage{ts.gam(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export
ts.gam<-function(x) {ifelse(length(x[x>0])<2,0,ks.test(x[which(x>0)], 'pgamma', MASS:::fitdistr(x[which(x>0)], 'gamma')$estimate[1],MASS:::fitdistr(x[which(x>0)], 'gamma')$estimate[2])$p.value)}

#' ts.lnorm
#'
#' \code{ts.lnorm}  tests empiric distribution versus lognormal theoretical distribution
#'
#'
#'\usage{ts.lnorm(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export
ts.lnorm<-function(x) {ifelse(length(x[x>0])<2,0,ks.test(x[which(x>0)], 'plnorm', MASS:::fitdistr(x[which(x>0)], 'lognormal')$estimate[1],MASS:::fitdistr(x[which(x>0)], 'gamma')$estimate[2])$p.value)}

#' ts.nor
#'
#' \code{ts.nor}  tests empiric distribution versus Normal theoretical distribution
#'@description function is a wrapper for KS normality test... TBA
#'
#'\usage{ts.nor(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export
ts.nor<-function(x) {ifelse(length(x[x>0])<2,0,ks.test(x[which(x>0)], 'pnorm', mean(x[which(x>0)]),sd(x[which(x>0)]))$p.val)}

#' ts.geom
#'
#' \code{ts.geom}  tests empiric distribution versus Geometric theoretical distribution
#'
#'\usage{ts.geom(x)}
#' @param x a vector containing observations, e.g. game densities
#' @return p-value of Kolmogorov-Smirnov test, computed by \code{link\{ks.test}}
#' @details Here will be described how it works TBA
#' @export
ts.geom<-function(x) {ifelse(length(x[x>0])<2,0,ks.test(x[which(x>0)], 'pgeom', fitdistr(x[which(x>0)], 'geometric')$estimate[1])$p.val)}
