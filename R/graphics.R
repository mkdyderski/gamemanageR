#' cdensplot
#'
#' \code{cdensplot}  function dedicated for quick single-region data obs.
#'
#'\usage{cdensplot(x)}
#' @param x .csv file exported from Excel, see details below
#' @return
#'\describe{
#'\item{value}{ggplot showing results for each method for considered species}}
#' @details Here will be described how it works TBA
#' @export
#'

cdensplot<-function(cdens){
  wyk2<- ggplot2:::ggplot(cdens, ggplot2:::aes(x=reorder(distribution,se), y=mean))+ggplot2:::geom_point(size=3)+ggplot2:::geom_segment(ggplot2:::aes(x=distribution,xend=distribution, y=mean-se, yend=mean+se))+ggplot2:::coord_flip()+ggplot2:::theme_bw()
  wyk2+ggplot2:::labs(x='Method', y='Density [ind. per 1000 ha]')+ggplot2:::ylim(0,NA) }

#' dplotall
#'
#' \code{dplotall}  function dedicated for plotting all species
#'
#'\usage{dplotall(x)}
#' @param x data frame containing game densities, e.g. result of \code{game.load}
#' @param name name for .pdf file which will be saved
#' @return
#'\describe{
#'\item{value}{ggplot showing results for each method for considered species}}
#' @details Here will be described how it works TBA
#' @export
#'

dplotall<-function(x, name){
ryswyk<-function(i){
  dupa<-cdensplot(compdens(x[,i],x[,2]))+ggplot2:::ggtitle(colnames(x)[i])
  print(dupa)
}

filnam<-paste(name, '.pdf')
pdf(filnam, paper = 'a4r')
sapply(3:ncol(x), ryswyk)
dev.off()
}

#' rysuj
#'
#' \code{rysuj}  polish wrapper for plotting all species
#'
#'\usage{rysuj(x)}
#' @param file name
#' @return
#'\describe{
#'\item{value}{ggplot showing results for each method for considered species}}
#' @details Here will be described how it works TBA
#' @export
#'

rysuj<-function(x, spe=';',long=TRUE){
  #load data, depndly on format
  if(long==TRUE)      dfram<-game.load.pl(x, sep=';')      else #jak długa
    dfram<- game.load(x, sep=';')
  #
  ryswyk<-function(i){
    #wwrrite basic functions for drawing
    ryswyk<-function(i){
      dupa<-cdensplot(compdens(dfram[,i],dfram[,2]))+ggplot2:::ggtitle(colnames(dfram)[i])
    }
    pl1<-ryswyk(i)

    duppa<-data.frame(obs=dfram[,i]*10)
    pl2<-ggplot2:::ggplot(duppa,ggplot2:::aes(x=obs))+ggplot2:::geom_histogram(fill='gray',col='black',binwidth =50)+ggplot2:::theme_bw()+ggplot2:::labs(x='Density', y='Number of observations')
    pl3<-ggplot2:::ggplot(duppa,ggplot2:::aes(x=obs))+ggplot2:::geom_density(fill='gray',col='black',adjust=2)+ggplot2:::theme_bw()+ggplot2:::labs(x='Density [ind. / 1000 ha]', y='Density function response')

    library(grid)
    # Move to a new page
    grid:::grid.newpage()

    # Create layout : nrow = 2, ncol = 2
    grid:::pushViewport(grid:::viewport(layout = grid.layout(2, 2)))

    # A helper function to define a region on the layout
    define_region <- function(row, col){
      grid:::viewport(layout.pos.row = row, layout.pos.col = col)
    }
    # Arrange the plots
    print(pl1, vp=define_region(1, 1:2))
    print(pl2, vp = define_region(2, 1))
    print(pl3, vp = define_region(2, 2))
  }
name<-paste('wykres_',x)
  filnam<-paste(name, '.pdf')
  pdf(filnam, paper = 'a4r')
  sapply(3:ncol(dfram), ryswyk)
  dev.off()
}
