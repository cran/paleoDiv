##Function ggcol()
#'Replicate the standard color scheme from ggplot2
#'
#' @param n Length of color vector to return.
#' @return A character vector containing color hex codes.
#' @importFrom grDevices hcl
#' @export ggcol
#' @examples
#' ggcol(3)

ggcol <- function(n) {
  h = seq(15, 375, length = n + 1)
  hcl(h = h, l = 65, c = 100)[1:n]
}
##


##Function add.alpha()
#'Add transparency to any color
#'
#' @param col Color value or vector of colors
#' @param alpha Opacity value to apply to the color(s)
#' @return A character vector containing color hex codes.
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#' @export add.alpha
#' @examples
#' add.alpha("red",0.8)

add.alpha <- function(col, alpha=0.5){
  if(missing(col))
    stop("Please provide a vector of colors.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}
##


##Function darken()
#'Darken or lighten colors by adding/subtracting to or hsv channel values
#'
#' @param x Color value or vector of colors
#' @param add Value to be added to the third hsv-channel. Can be a vector of length x, or a vector of any length if length(x)==1
#' @param abs Value to substitute for the third hsv-channel. If set, this overrides the setting for parameter add. Can be a vector of length x, or a vector of any length if length(x)==1
#' @return A color value or vector of color values of length x (or, if length(x)==1, the length of add or abs)
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb2hsv
#' @importFrom grDevices hsv
#' @export darken
#' @examples
#' darken(ggcol(3),abs=0.5)

darken<-function(x, add=0, abs=NULL){

darken_<-function(col, add=0, abs=NULL){
  if(missing(col)){
    stop("Please provide a vector of colors.")}
    
    character(length(col))->out
    
    for(i in 1:length(col)){
    rgb2hsv(col2rgb(col[i]))->tmpcol
    
    if(!is.null(abs)){
    tmpcol[3]<-abs
    }else{
    if(tmpcol[3]+add<0){
    tmpcol[3]<-0
    }else if(tmpcol[3]+add>1){
    tmpcol[3]<-1
    }else{    
    tmpcol[3]<-tmpcol[3]+add}
    }
    hsv(tmpcol[1], tmpcol[2], tmpcol[3])->out[i]
    
    }
    
    return(out)
 
}

if(length(add)==1 & is.null(abs)){#standard use case, if add is given as a single value
darken_(x,add)
}else if(!is.null(abs) & length(abs)==1){#if abs is given as a single value
darken_(x,abs=abs)
}else if(!is.null(abs) & length(abs)>1 & length(x)==1){#if abs is a vector and color is only a single value
length(abs)->n
character(n)->out
for(i in 1:n){
out[i]<-darken_(x, abs=abs[i])}
return(out)
}else if(length(add)>1 & is.null(abs) & length(x)==1){#if add is given as a vector and color is only a single value
length(add)->n
character(n)->out
for(i in 1:n){
out[i]<-darken_(x, add=add[i])}
return(out)
}else if(length(add)>1 & is.null(abs) & length(x)>1){#if add is given as a vector and x is a vector too
length(x)->n
character(n)->out
if(length(add)!=n){stop("color and change vectors need to be the same length")}

for(i in 1:n){
out[i]<-darken_(x[i], add=add[i])}
return(out)
}else if(!is.null(abs) & length(abs)>1 & length(x)>1){#if abs is given and a vector, and x is a vector too
length(x)->n
character(n)->out
if(length(abs)!=n){stop("color and change vectors need to be the same length")}
for(i in 1:n){
out[i]<-darken_(x[i], abs=abs[i])}
return(out)
}

}
###





##Function rmean()
#'Calculate a rolling mean for a vector x.
#'
#' @param x Numeric vector for which to calculate the rolling mean.
#' @param width Width of the interval over which to calculate rolling mean values. Should be an uneven number (even numbers are coerced into the next-higher uneven number)
#' @return A numeric vector of the same length as x containing the calculated rolling means, with the first and last few values being NA (depending on the setting for width)
#' @export rmean
#' @examples
#' rmean(x=c(1,2,3,4,5,6),width=5)

rmean<-function(x, width=11){
x_<-rep(NA,length(x))

for(i in ceiling(width/2):(length(x)-round((width-1)/2))){

i-(ceiling((width-1)/2))->lwr
i+(ceiling((width-1)/2))->upr

j<-seq(lwr,upr,1)
x_[i]<-mean(x[j],na.rm=TRUE)
}

return(x_)
}
##



##Function rmeana()
#'Calculate a rolling mean based on distance within a second variable.
#'
#' @param x0 Numeric independent variable at which rolling mean is to be calculated.
#' @param y0 Numeric variable of which mean is to be calculated.
#' @param x1 Optional. New x values at which rolling mean of y0 is to be calculated. If x1==NULL, calculation will take place at original (x0) values.
#' @param plusminus Criterium for the width (in x0) of the interval over which rolling mean values are to be calculated. Value represents the margin as calculated from every value of x1 or x0, i.e. for a plusminus==5, the interval over which the means are drawn will range from values with x-x_i=5 to x-x_i=-5.
#' @param weighting Whether or not to apply weighting. If weighting==TRUE, then means are calculated as weighted means with weighting decreasing linearly towards the margins of the interval over which the mean is to be drawn.
#' @param weightdiff Minimum weight to be added to all weights if weighting==TRUE. Defaults to 0.
#' @return A numeric vector of the same length as either x1 (if not NULL) or x0, containing the calculated rolling means.
#' @importFrom stats weighted.mean
#' @export rmeana
#' @examples
#' rmeana(x0=c(1,2,3,4,5,6), y0=c(2,3,3,4,5,6))

rmeana<-function(x0, y0, x1=NULL, plusminus=5, weighting=FALSE,weightdiff=0){

if(is.null(x1)){
x_<-x0
}else{
x_<-x1}

y_<-rep(NA,length(x_))

for(i in 1:length(x_)){

indices<-which(abs(as.numeric(x0-x_[i]))<=plusminus)

if(weighting==TRUE){

weights<-abs(as.numeric(x0[indices]-x_[i]))

y_[i]<-weighted.mean(y0[indices],w=plusminus+weightdiff-weights,na.rm=TRUE)

}else{
y_[i]<-mean(y0[indices],na.rm=TRUE)}
}

return(y_)
}
##


##Function viol 
#'Generate a violin plot
#'
#' @param x Variable for which to plot violin.
#' @param pos Position at which to place violin in the axis perpendicular to x
#' @param x2 Optional variable to use instead of x as input variable for the violin plot. If x2 is set, the function (default: density()) used to calculate the plotting statistic is run on x2 instead of x, but the results are plotted at the corresponding x values.
#' @param stat The plotting statistic. Details to the density() function, as in a standard violin plot, but can be overridden with another function that can take x or x2 as its first argument. Stat can also be a numeric vector of the same length as x, in which case the values in this vectors are used instead of the function output and plotted against x as an independent variable.
#' @param dscale The scale to apply to the values for density (or another plotting statistic). Defaults to 1, but adjustment may be needed depending on the scale of the plot the violin is to be added to.
#' @param cutoff Setting for cropping the violin. Can be either a single value, in which case the input is interpreted as number of standard deviations from the mean, or a numeric vector of length 2, giving the lower and upper cutoff value directly.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param horiz Logical indicating whether to plot horizontally (defaults to TRUE) or vertically
#' @param add Logical indicating whether to add to an existing plot (defaults to TRUE) or generate a new plot.
#' @param lim Limits (in the dimensions of x) used for plotting, if add==FALSE. Defaults to cutoff, but can be manually set as a numeric vector of length 2, giving the lower and upper limits of the plot.
#' @param fill Fill color for the plotted violin
#' @param col Line color for the plotted violin
#' @param lwd Line width for the plotted violin
#' @param lty Line width for the plotted violin
#' @param ... Other arguments to be passed on to function in parameter stat
#' @return A violin plot and a data.frame containing the original and modified plotting statistic and independent variable against which it is plotted.
#' @details
#' Viol provides a versatile function for generating violin plots and adding them to r base graphics. The default plotting statistic is density(), resulting in the standard violin plot. However, density can be overridden by entering any function that can take x or x2 as its first argument, or any numeric vector containing the data to be plotted, as long as this vector is the same length as x.
#' @importFrom stats density sd
#' @importFrom graphics polygon
#' @export viol
#' @examples
#' viol(x=c(1,2,2,2,3,4,4,3,2,2,3,3,4,5,3,3,2,2,1,6,7,6,9),pos=1, add=FALSE)
#' viol(c(1:10), width=9, stat=rmean, pos=0, add=FALSE)
#' viol(c(1:10), stat=c(11:20), pos=0, add=FALSE)

viol<-function(x, pos, x2=NULL, stat=density, dscale=1, cutoff=range(x), horiz=TRUE, add=TRUE,lim=cutoff,xlab="",ylab="", fill="grey", col="black", lwd=1, lty=1,...){
#sort ascending
if(!is.null(x2) & is.numeric(x2) & length(x2)==length(x)){
x2[order(x)]->x2
}
x[order(x)]->x

#calculate plotting statistic. defaults to density, but other functions can be used by altering the stat parameter
if(is.numeric(stat)){#if stat is a vector 
if(length(stat)==length(x)){
stat->d}else{stop("If stat is numeric(), it has to be the same length as x")}
}else{#if stat is a function
if(!is.null(x2) & length(x2)==length(x)){
stat(x2,...)->d
}else{
stat(x,...)->d
}
}

if(is.numeric(d)){#if d is a numeric vector, i.e. not a data.frame() as is output by density()
d<-data.frame(y=d, x=x)#make data.frame if not already existing. Otherwise it is assumed that there are d$y (giving the plotting statistic) and d$x (giving the independent variable) in the object
}

if(length(cutoff)==1){#if z score is given for cutoff, convert it into range around mean
cutoff<-c(mean(x,na.rm=TRUE)-cutoff*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+cutoff*sd(x,na.rm=TRUE))
}

#now crop data range to match cutoff range
as.numeric(d$y[which(d$x<=max(cutoff) & d$x>=min(cutoff))])->dstat0
dstat0[!is.na(dstat0)]->dstat0_#remove NAs
dstat0_/2*dscale->dstat#scaled statistic to contruct violin

as.numeric(d$x[which(d$x<=max(cutoff) & d$x>=min(cutoff))])->xstat
xstat[!is.na(dstat0)]->xstat
#independent variable

##plotting
if(add==TRUE){#add to an existing plot
if(horiz==TRUE){#plot horizontally
polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd,lty=lty)
}else(#plot vertically
polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd,lty=lty)
)

}else{#create a new plot
if(horiz==TRUE){
plot(density(x), xlim=lim, ylim=c(pos+max(dstat), pos-max(dstat)), type="n", ylab=ylab, xlab=xlab,main="")

polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd,lty=lty)

}else{
plot(density(x), ylim=lim, xlim=c(pos+max(dstat), pos-max(dstat)), type="n", xlab=xlab, ylab=ylab,main="")

polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd,lty=lty)
}
}

#return invisible plot object
invisible(data.frame(o_statistic=c(dstat0_,rev(dstat0_)),plot_statistic=c(pos+dstat,rev(pos-dstat)),independent=c(xstat,rev(xstat))))

}
##

##Function tsconv
#'Convert geological ages for accurate plotting alongside a calibrated phylogeny
#'
#' @param x A vector of geological ages to be converted.
#' @param phylo0 Phylogeny from which to take root.age
#' @param root.time Numeric root age, if not taken from a phylogeny
#' @return A numeric() containing the converted geological ages
#' @export tsconv
#' @examples
#' tsconv(c(252,201,66), root.time=300)
tsconv<-function(x,phylo0=NULL,root.time=phylo0$root.time){
-1*(x-root.time)
}
##

##Function ts.periods 
#'Add a horizontal, period-level phanerozoic timescale to any plot, especially calibrated phylogenies plotted with ape.
#'
#' @param phylo Optional (calibrated) phylogeny to which to add timescale. If phylogeny is provided, the $root.time variable is used to convert ages so that the time scale will fit the phylogeny.
#' @param alpha Opacity value to use for the fill of the time scale
#' @param names Logical indicating whether to plot period names (defaults to TRUE)
#' @param exclude Character vector listing periods for which to not plot the names, if names==TRUE
#' @param col.txt Color(s) to use for labels.
#' @param border Color to use for the border of the timescale
#' @param ylim Setting for height of the timescale. Can either be one single value, in which case the function attempts to use the lower limit of the currently plotted phylogeny, or a vector of length 2 containing the lower and upper limits of the timescale.
#' @param adj.txt Numeric vector of length==2 giving horizontal and vertical label alignment (defaults to centered, i.e. 0.5 for both values)
#' @param txt.y Function to use to determine the vertical text position (defaults to mean, i.e. centered) 
#' @param bw Logical whether to plot in black and white (defaults to FALSE). If TRUE, time scale is drawn with a white background
#' @param update Character string giving the filename of a .csv table for providing an updated timescale. If provided, the values for plotting the time scale are taken from the csv file instead of the internally provided values. Table must have columns named periods, bottom, top and col, giving the period names, start time in ma, end time in ma and a valid color value, respectively.
#' @return Plots a timescale on the currently active plot.
#' @importFrom graphics text
#' @importFrom utils read.csv
#' @importFrom ape plot.phylo
#' @export ts.periods
#' @examples
#' data(tree_archosauria)
#' ape::plot.phylo(tree_archosauria)
#' ts.periods(tree_archosauria, alpha=0.5)

ts.periods <- function(phylo=NULL,alpha=1,names=TRUE,exclude=c("Quarternary"),col.txt=NULL,border=NA,ylim=0.5,adj.txt=c(0.5,0.5),txt.y=mean,bw=FALSE,update=NULL){
  ## Data for geological periods
  
  if(!is.null(update)){
  read.csv(update)->ts #use this to manually update time scale
  periods<-data.frame(interval=ts$periods,start=ts$bottom, end=ts$top,col=ts$col)
  
  }else{
  periods <- data.frame(
    period = c("Quarternary", "Neogene", "Paleogene", "Cretaceous", "Jurassic", "Triassic", "Permian", "Carboniferous", "Devonian", "Silurian", "Ordovician", "Cambrian","Ediacaran"),
    start = c(0, 2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4,541),
    end = c(2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4, 541,635), col = paste0("#",c("F9F97F","FFE619","FD9A52","7FC64E","34B2C9","812B92","F04028","67A599","CB8C37","B3E1B6","009270","7FA056","FED96A")))
  }

  if(!is.null(phylo)){
periods$start<-tsconv(periods$start,phylo)
periods$end<-tsconv(periods$end,phylo)
  }

  
  if(is.null(col.txt)){col.txt<-periods$col}#set text color, is unset

if(length(ylim)==1 & !is.null(phylo)){
lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
c(min(lastPP$y.lim),ylim)->ylim
}else if(length(ylim)==1){ylim<-c(0,ylim)}
  
  for (i in 1:nrow(periods)) {
    if(bw!=TRUE){polygon(
      x=c(periods$start[i], periods$end[i], periods$end[i], periods$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = add.alpha(periods$col[i],alpha=alpha),
      border = border
    )}else if(bw==TRUE){
    polygon(
      x=c(periods$start[i], periods$end[i], periods$end[i], periods$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = "white",
      border = border
    )
    }
    
    if(names==TRUE){
    if(periods$period[i] %in% exclude == FALSE){
    
    if(length(col.txt)>1){#if text colors are given as vector
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt[i])}else{#if single text color is given
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt)
    }
    }
    
    }
  }

}
##



##Function ts.stages
#'Add a horizontal, stage-level phanerozoic timescale to any plot, especially calibrated phylogenies plotted with ape.
#'
#' @param phylo Optional (calibrated) phylogeny to which to add timescale. If phylogeny is provided, the $root.time variable is used to convert ages so that the time scale will fit the phylogeny.
#' @param alpha Opacity value to use for the fill of the time scale
#' @param names Logical indicating whether to plot stage names (defaults to FALSE)
#' @param col.txt Color(s) to use for labels.
#' @param border Color to use for the border of the timescale
#' @param ylim Setting for height of the timescale. Can either be one single value, in which case the function attempts to use the lower limit of the currently plotted phylogeny, or a vector of length 2 containing the lower and upper limits of the timescale.
#' @param adj.txt Numeric vector of length==2 giving horizontal and vertical label alignment (defaults to centered, i.e. 0.5 for both values)
#' @param txt.y Function to use to determine the vertical text position (defaults to mean, i.e. centered) 
#' @param bw Logical whether to plot in black and white (defaults to FALSE). If TRUE, time scale is drawn with a white background
#' @param update Character string giving the filename of a .csv table for providing an updated timescale. If provided, the values for plotting the time scale are taken from the csv file instead of the internally provided values. Table must have columns named stage, bottom, top and col, giving the stage names, start time in ma, end time in ma and a valid color value, respectively.
#' @return Plots a timescale on the currently active plot.
#' @importFrom graphics text
#' @importFrom utils read.csv
#' @importFrom ape plot.phylo
#' @export ts.stages
#' @examples
#' data(tree_archosauria)
#' ape::plot.phylo(tree_archosauria)
#' ts.stages(tree_archosauria, alpha=0.7)
#' ts.periods(tree_archosauria, alpha=0)


ts.stages <- function(phylo=NULL,alpha=1,names=FALSE,col.txt=NULL,border=NA,ylim=0.5,adj.txt=c(0.5,0.5),txt.y=mean,bw=FALSE,update=NULL){
  ##Data for geological periods
  if(!is.null(update)){
  read.csv(update)->ts #use this to manually update time scale
  intervals<-data.frame(interval=ts$stage,start=ts$bottom, end=ts$top,col=ts$col)
  }else{
    intervals<-data.frame(interval=c('Avalon Assemblage', 'White Sea Assemblage', 'Nama Assemblage', 'Fortunian', 'Stage 2', 'Stage 3', 'Stage 4', 'Wulian', 'Drumian', 'Guzhangian', 'Paibian', 'Jiangshanian', 'Stages 10', 'Tremadocian', 'Floian', 'Dapingian', 'Darriwilian', 'Sandbian', 'Katian', 'Hirnantian', 'Rhuddanian', 'Aeronian', 'Telychian', 'Sheinwoodian', 'Homerian', 'Gorstian', 'Ludfordian', 'Pridoli', 'Lochkovian', 'Pragian', 'Emsian', 'Eifelian', 'Givetian', 'Frasnian', 'Famennian', 'Tournaisian', 'Visean', 'Serpukhovian', 'Bashkirian', 'Moscovian', 'Kasimovian', 'Gzhelian', 'Asselian', 'Sakmarian', 'Artinskian', 'Kungurian', 'Roadian', 'Wordian', 'Capitanian', 'Wuchiapingian', 'Changhsingian', 'Induan', 'Olenekian', 'Anisian', 'Ladinian', 'Carnian', 'Norian', 'Rhaetian', 'Hettangian', 'Sinemurian', 'Pliensbachian', 'Toarcian', 'Aalenian', 'Bajocian', 'Bathonian', 'Callovian', 'Oxfordian', 'Kimmeridgian', 'Tithonian', 'Berriasian', 'Valanginian', 'Hauterivian', 'Barremian', 'Aptian', 'Albian', 'Cenomanian', 'Turonian', 'Coniacian', 'Santonian', 'Campanian', 'Maastrichtian', 'Danian', 'Selandian-Thanetian', 'Ypresian', 'Lutetian', 'Bartonian', 'Priabonian', 'Rupelian', 'Chattian', 'Lower Miocene', 'Middle Miocene', 'Upper Miocene', 'Pliocene', 'Pleistocene', 'Holocene'),start=c(580, 560, 550, 538.8, 529, 521, 514.5, 509, 504.5, 500.5, 497, 494.2, 491, 486.85, 477.08, 471.26, 469.42, 458.18, 452.75, 445.21, 443.07, 440.49, 438.59, 432.93, 430.62, 426.74, 425.01, 422.73, 419, 412.4, 410.51, 394.3, 385.3, 378.9, 371.1, 359.3, 346.73, 330.34, 323.4, 315.15, 307.02, 303.68, 298.89, 293.52, 290.51, 283.3, 274.37, 269.21, 264.34, 259.55, 254.24, 251.9, 249.88, 246.7, 241.46, 237, 227.3, 209.51, 201.36, 199.46, 192.9, 184.2, 174.7, 170.9, 168.17, 165.29, 161.53, 154.78, 149.24, 143.1, 137.7, 132.6, 126.5, 121.4, 113.2, 100.5, 93.9, 89.39, 85.7, 83.65, 72.17, 66.04, 61.66, 56, 48.07, 41.03, 37.71, 33.9, 27.29, 23.04, 15.99, 11.63, 5.33, 2.59, 0.0117), end=c(560, 550, 538.8, 529, 521, 514.5, 509, 504.5, 500.5, 497, 494.2, 491, 486.85, 477.08, 471.26, 469.42, 458.18, 452.75, 445.21, 443.07, 440.49, 438.59, 432.93, 430.62, 426.74, 425.01, 422.73, 419, 412.4, 410.51, 394.3, 385.3, 378.9, 371.1, 359.3, 346.73, 330.34, 323.4, 315.15, 307.02, 303.68, 298.89, 293.52, 290.51, 283.3, 274.37, 269.21, 264.34, 259.55, 254.24, 251.9, 249.88, 246.7, 241.46, 237, 227.3, 209.51, 201.36, 199.46, 192.9, 184.2, 174.7, 170.9, 168.17, 165.29, 161.53, 154.78, 149.24, 143.1, 137.7, 132.6, 126.5, 121.4, 113.2, 100.5, 93.9, 89.39, 85.7, 83.65, 72.17, 66.04, 61.66, 56, 48.07, 41.03, 37.71, 33.9, 27.29, 23.04, 15.99, 11.63, 5.33, 2.59, 0.0117, 0),col=c('#fcd589', '#fdd587', '#fed583', '#a9be93', '#b6c29e', '#b5ca9f', '#c0ceaa', '#c1d6af', '#cddbb8', '#d6e1c1', '#d8e8c4', '#e4efcf', '#edf2db', '#0dac98', '#0eb1a0', '#67c0ae', '#79c5b8', '#9bceaf', '#a6d5c3', '#b6d9c3', '#b3dccc', '#c1e1d6', '#cae8e0', '#cce7d8', '#d6ebe2', '#d6ecea', '#e2f1ec', '#ebf5ec', '#eac378', '#ebcd87', '#edd595', '#f5da93', '#f5e2a0', '#f4edc3', '#f4f0d5', '#9db989', '#b7c089', '#cdc888', '#a6c9cd', '#bed3ce', '#cad8d9', '#d6dcda', '#e07f6c', '#e18a76', '#e39684', '#e49f90', '#f49984', '#f4a692', '#f6af9b', '#fac4b8', '#facfc6', '#b266a6', '#bb71ac', '#c793c3', '#d0a0c8', '#d1b3d5', '#dbc1de', '#e5cbe4', '#22b5e9', '#5ebeee', '#86c9f3', '#a3d1f3', '#a2d8f0', '#b0dff1', '#bce3f2', '#cae6f2', '#cce8fd', '#d4eefd', '#e0f2fc', '#9ec979', '#a8d182', '#b7d690', '#c2da9c', '#cfe1a7', '#d9e8b1', '#c6d86c', '#d2dd77', '#dce383', '#e5e88f', '#efec9b', '#f5f1a7', '#fbc27f', '#fccb87', '#f7ba8e', '#f9c39f', '#fcceac', '#fcd7ba', '#fedfb3', '#ffeac3', '#fff14a', '#fef26b', '#fef488', '#fff7b2', '#fff2c5', '#fff5eb'))
}
  
  if(!is.null(phylo)){
intervals$start<-tsconv(intervals$start,phylo)
intervals$end<-tsconv(intervals$end,phylo)
  }
  
  
  if(is.null(col.txt)){
    col.txt<-intervals$col}
    
if(length(ylim)==1 & !is.null(phylo)){
lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
c(min(lastPP$y.lim),ylim)->ylim
}else if(length(ylim)==1){ylim<-c(0,ylim)}

  for (i in 1:nrow(intervals)) {
    ###
    if(bw!=TRUE){polygon(
      x=c(intervals$start[i], intervals$end[i], intervals$end[i], intervals$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = add.alpha(intervals$col[i],alpha=alpha),
      border = border
    )}else if(bw==TRUE){
    polygon(
      x=c(intervals$start[i], intervals$end[i], intervals$end[i], intervals$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = "white",
      border = border
    )
    ###
    }
    
    if(names==TRUE){
    if(length(col.txt)>1){
    text(x=mean(c(intervals$start[i],intervals$end[i])),y=txt.y(ylim), adj=adj.txt,intervals$interval[i],col=col.txt[i])}else{
    text(x=mean(c(intervals$start[i],intervals$end[i])),y=txt.y(ylim), adj=adj.txt,intervals$interval[i],col=col.txt)
    }
    }
  }

}




##Function redraw.phylo
#'Redraw the lines of a phylogenetic tree.
#'
#' @param saved_plot Optional saved plot (e.g. using get("last_plot.phylo", envir = ape::.PlotPhyloEnv)) to be used instead of currently active plot.
#' @param col Color to be used for redrawing tree edges.
#' @param lwd Line width to be used for redrawing tree edges.
#' @param lty Line type to be used for redrawing tree edges.
#' @param lend Style of line ends to be used for redrawing tree edges.
#' @param arrow.l Length of arrow ends to be used for plotting. Defaults to 0, i.e. no visible arrow.
#' @param arrow.angle Angle of arrow ends to be used for plotting. Defaults to 45 degrees.
#' @param arrow.code Arrow code to be used for plotting. For details, see ?arrows
#' @param indices Optional indices which edges to redraw. Can be used to highlight specific edges in different color or style.
#' @return Plots a timescale on the currently active plot.
#' @importFrom graphics arrows
#' @importFrom ape plot.phylo
#' @export redraw.phylo
#' @examples
#' data(tree_archosauria)
#' ape::plot.phylo(tree_archosauria)
#' redraw.phylo(col="darkred",lwd=3,indices=c(19:24))
#' redraw.phylo(col="red",lwd=3,indices=c(18),arrow.l=0.1)

redraw.phylo<-function(saved_plot=NULL,col="black",lwd=1,lty=1,lend=2,arrow.l=0, arrow.angle=45, arrow.code=2, indices=NULL){
#load data
if(is.null(saved_plot)){
lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
}else(lastPP<-saved_plot)

if(!is.null(indices)){
lastPP$edge[indices,]->lastPP$edge

if(length(indices==1)){rbind(lastPP$edge,c(NA,NA))->lastPP$edge}#retain data.frame format if only one index given

}
#now loop through edges
for(i in 1:nrow(lastPP$edge)){
lastPP$edge[i,1]->starti
lastPP$edge[i,2]->endi
if(lastPP$type=="cladogram"){
arrows(lastPP$xx[starti],lastPP$yy[starti],lastPP$xx[endi],lastPP$yy[endi],col=col,lwd=lwd, lty=lty,lend=lend,length=arrow.l,angle=arrow.angle, code=arrow.code)

}else if(lastPP$type=="phylogram"){
arrows(lastPP$xx[starti],lastPP$yy[starti],lastPP$xx[starti],lastPP$yy[endi],length=0,col=col,lwd=lwd, lty=lty,lend=lend)
arrows(lastPP$xx[starti],lastPP$yy[endi],lastPP$xx[endi],lastPP$yy[endi],col=col,lwd=lwd, lty=lty,lend=lend,length=arrow.l,angle=arrow.angle, code=arrow.code)
}else{
stop("Only phylogram and cladogram supported so far")
}
}

}
##

##Function pdb
#'Download data from the paleobiology database.
#'
#' @param taxon A taxon (base_name) for which to download records.
#' @param interval A character string indicating over which temporal interval to download data (defaults to "all"), e.g. "Phanerozoic" or "Jurassic".
#' @param what  The type of data to download (for details, see https://paleobiodb.org/data1.2/). Defaults to "occs", which downloads occurrence data. Setting this parameter to "colls" will instead download collection data.
#' @param full A logical indicating whether or not the full dataset is to be downloaded (defaults to FALSE). At the expense of larger file size, the full dataset contains a large number of additional columns containing data such as stratigraphy, phylogeny and (paleo)geography, which is useful for various purposes but not strictly necessary for graphing paleodiversity.
#' @param base Character string containing base url to use. Defaults to https://paleobiodb.org/data1.2/. Entering "dev" serves as a shortcut to use https://dev.paleobiodb.org/data1.2/ instead (can sometimes be helpful if one of the two is unavailable).
#' @param file Character string containing which file name to look for. Defaults to list.csv.
#' @return A data.frame() containing the downloaded paleobioDB dataset. The column "identified_name" will be copied into the column "tna", and (if what==occs) the columns "max_ma" and "min_ma" will be copied into the columns named "eag" and "lag" respectively, maintaining compatibility with the output of the deprecated package "paleobioDB" for those variable names.
#' @importFrom utils read.csv
#' @export pdb
#' @examples
#' pdb("Stegosauria")->Stegosauria

pdb<-function(taxon, interval="all", what="occs", full=FALSE, base="https://paleobiodb.org/data1.2/",file="list.csv"){

if(base=="dev"){base<-"https://dev.paleobiodb.org/data1.2/"}

if(full==TRUE){
    if(interval=="all"){
    pbdb_url <-paste0(base,what,"/",file,"?base_name=",taxon,"&show=full")
    }else{
    pbdb_url <-paste0(base,what,"/",file,"?base_name=",taxon,"&interval=",interval,"&show=full")}
}else{
    if(interval=="all"){
    pbdb_url <-paste0(base,what,"/",file,"?base_name=",taxon)
    }else{
    pbdb_url <-paste0(base,what,"/",file,"?base_name=",taxon,"&interval=",interval)}
    }


#occ<-read.csv(pbdb_url)

tryCatch({read.csv(pbdb_url)}, error=function(e){
return(paste("error in paleodb query:", e$message))

},warning=function(w){
return(paste("warning in paleodb query:", w$message))

})->occ


if(is.data.frame(occ)){
    if(what=="occs"){
occ$identified_name->occ$tna#for colname compatibility with deprecated paleobiodb package
}

occ$hltax<-taxon#higher level taxon for categorization,e.g. if tables are combined via rbind
occ$min_ma->occ$lag#for colname compatibility with deprecated paleobiodb package
occ$max_ma->occ$eag#for colname compatibility with deprecated paleobiodb package
return(occ)

}else if(length(occ)==1){
message(occ)
if(occ=="error in paleodb query: more columns than column names"){message("This probably means that the taxonomic name you entered (",taxon,") could not be found on paleobiodb.org.")}
if(grepl("Timeout",occ) | grepl("502 Bad Gateway",occ)){message("It seems that the server at paleobiodb.org/data1.2/ is not responding at the moment - please try again later.")}
if(grepl("resolve host name",occ)){message("Please check your internet connection.")}

invisible(occ)
}

}


##Function mk.sptab
#'Generate a taxon-range table based on an occurrence dataset.
#'
#' @param xx A data.frame() of occurrence records, containing at least the following columns: taxonomic name at level at which ranges are to be determined (e.g. species or genus), earliest possible age for each occurrence and latest possible age for each occurrence. If xx==NULL, then each column or vector must be specified individualy using the following parameters
#' @param taxa column/vector containing the taxonomic variable. Defaults to xx$tna
#' @param earliest column/vector containing the earliest age estimate. Defaults to xx$eag.
#' @param latest column/vector containing the latest age estimate. Defaults to xx$lag.
#' @param tax Optional. A single character string containing the taxon name, to be added as another column to the range table (useful for categorization, should several range tables be concatenated, e.g. using rbind()).
#' @return A data.frame() containing the taxon names, the maximum and minimum age for each taxon, and (optionally) a column with the name of the higher-level taxon.
#' @export mk.sptab
#' @examples
#' data(archosauria)
#' mk.sptab(archosauria$Stegosauria)->sptab_Stegosauria


mk.sptab<-function(xx=NULL,taxa=xx$tna, earliest=xx$eag, latest=xx$lag, tax=NULL){
sptab<-levels(factor(taxa))
n<-length(sptab)

xx<-data.frame(tna=taxa, lag=as.numeric(latest), eag=as.numeric(earliest))

sptab<-data.frame(tna=sptab, max=rep(NA,n), min=rep(NA,n))#make table with age ranges for all species (or other taxa)

for(i in 1:n){
sptab$max[i]<-max(xx$eag[xx$tna==sptab$tna[i]])
sptab$min[i]<-min(xx$lag[xx$tna==sptab$tna[i]])
}
(sptab$min+sptab$max)/2->sptab$ma
sptab$tax<-rep(tax,n)
return(sptab)
}
##



##Function synonymize
#'Combine selected entries in a taxon-range table to remove duplicates
#'
#' @param x Indices or values (taxon names) to combine
#' @param table Taxon-range table
#' @param ids Vector or column of taxon names (used for matching taxon names in x). Defaults to table$tna
#' @param max Vector or column containing maximum ages
#' @param min Vector or column containing minimum ages
#' @return A data.frame containing taxon names, maximum, minimum and mean ages, with ranges for the selected entries merged and superfluous entries removed (note that the first taxon indicated by x is kept as valid).
#' @details This function is meant as an aid to manually editing species tables and remove synonyms or incorrect spellings of taxonomic name that result in an inflated number of distinct taxa being represented.
#' @export synonymize
#' @examples
#' data(archosauria)
#' sp<-archosauria$sptab_Stegosauria
#' synonymize(c(32,33),sp)->sp
#' synonymize(grep("stenops",sp$tna),sp)->sp
#' synonymize(c("Hesperosaurus mjosi","Stegosaurus mjosi"),sp)->sp

synonymize<-function(x,table=NULL, ids=table$tna, max=table$max, min=table$min){
if(length(x)<2){stop("You must provide at least two taxon names or row numbers to synonymize!")}

if(!(length(ids) == length(max) & length(ids) == length(min))){
stop("ids, max and min must have the same number of elements!")}

c1<-!is.null(table) & is.data.frame(table) & sum(ids==table$tna) == length(ids) & sum(max==table$max) == length(max) & sum(min==table$min) == length(min) #test whether ids, min and max are columns of table

if(is.numeric(x)){
indices<-x
}else{
which(ids == x[1])->indices
indices<-c(indices, which(ids %in% x[2:length(x)]))
}

max_<-max(max[indices])
min_<-min(min[indices])

ids[indices]<-ids[indices[1]]
max[indices]<-max_
min[indices]<-min_

if(c1==TRUE){ #if table contains the vectors to be evaluated, replace them in the table
table$tna<-ids
table$max<-max
table$min<-min

table[-indices[c(2:length(indices))],]->table

}else{#otherwise, build new data.frame
table<-data.frame(tna=ids, max=max, min=min, ma=(min+max)/2)
table[-indices[c(2:length(indices))],]->table
}

rownames(table)<-c(1:nrow(table))

return(table)
}
##


##Function divdistr_
#'Calculate total species diversity for any point in time based on a taxon-range table
#'
#' @param x A point in time or vector of points in time, in ma, at which species diversity is to be determined.
#' @param table A taxon-range table to be used, usually the output of mk.sptab()
#' @param w A vector of weights to apply to the estimated (raw) diversity figures. This vector needs to be of the same length as x. Each raw diversity estimate will then be multiplied by the weight. Can be used to account for differences in collection intensity/sampling biases, if these can be quantified (e.g. by analyzing collection records.
#' @param smooth The smoothing margin, in units of ma. Corresponds to the plusminus parameter of rmeana(). Defaults to 0, i.e. no smoothing (beyond the resolution determined by the resolution of x)
#' @param max Vector or column containing the maximum age of each entry in the taxon-range table. Defaults to table$max
#' @param min Vector or column containing the minimum age of each entry in the taxon-range table. Defaults to table$min
#' @details divdistr_() produces a "maximum" estimate of taxonomic diversity at any given point in time in the fossil record. This function is based on the principle of counting the number of taxon ranges (from the provided range table) that overlap each age provided in x. As a result of uncertainty of age estimates, this may lead to an overestimation of the actual fossil diversity at each point in time, especially at the points of overlap between taxon-specific ranges. Moreover this represents a "raw", uncorrected diversity estimate that does not account for differences in sampling intensity throughout the time interval that is investigated. A rudimentary functionality for using such a correction exists in the form of the w argument, which allows the user to provide a vector of weights (of the same length as x) to be multiplied with the raw diversity estimates. Such weights can, for instance, be based on (the inverse of) the number of collections overlapping any given age in x, which can be calculated using the same basic approach as the raw diversity, by downloading collections instead of occurrence data.
#' @return A numeric vector containing taxon diversity (at the chosen taxonomic level used in the generation of the range table) at the provided ages.
#' @export divdistr_
#' @examples
#' data(archosauria)
#' divdistr_(c(170:140),table=archosauria$sptab_Stegosauria)
#' curve(divdistr_(x,archosauria$sptab_Stegosauria), xlim=c(200,100),ylim=c(-5,35))
#' ts.stages(ylim=c(-6,-1),alpha=0.3,border=add.alpha("grey"))
#' ts.periods(ylim=c(-6,-1),alpha=0.0)

divdistr_<-function(x, table=NULL,w=rep(1,length(x)),smooth=0, max=table$max, min=table$min){

    divdistr<-function(x,table=NULL, min=table$min, max=table$max){
which(min<=x)->a
which(max>=x)->b
intersect(a,b)->id
length(id)->length
return(length)
    }

length(x)->n
rep(NA,n)->tmp
for(i in 1:n){
divdistr(x[i], table=table)*w[i]->tmp[i]
}

if(smooth>0){tmp<-rmeana(y0=tmp,x0=x, plusminus=smooth)}
return(tmp)
}
##




##Function abdistr_
#'Count number of entries in occurrence or collection data.frame for specific points in geological time
#'
#' @param x A numeric vector giving the times (in ma) at which to determine the number of overlapping records.
#' @param table An occurrence or collection dataset
#' @param ab.val Abundance value to be used. Default is table$abund_value. If NULL (e.g. because this column does not exist) or NA, each occurrence is treated as representing one specimen
#' @param smooth The smoothing margin, in units of ma. Corresponds to the plusminus parameter of rmeana(). Defaults to 0, i.e. no smoothing (beyond the resolution determined by the resolution of x)
#' @param max Vector or column containing maximum age of each occurrence or collection
#' @param min Vector or column containing minimum age of each occurrence or collection
#' @param w A Vector of weights. Must be of same length as x
#' @return A numeric vector of the same length as x, giving the estimated number of occurrence records (if ab.val==FALSE) or specimens (if ab.val==TRUE), or the estimated number of collections (if collection data are used instead of occurrences) overlapping each temporal value given in x
#' @export abdistr_
#' @examples
#' data(archosauria)
#' abdistr_(x=c(170:120), table=archosauria$Stegosauria)

abdistr_<-function(x, table=NULL, ab.val=table$abund_value, smooth=0, max=table$eag, min=table$lag,w=rep(1,length(x))){

    abdistr<-function(x,table=NULL, ab.val=table$abund_value, max=table$eag, min=table$lag){

    if(is.null(ab.val)){ab.val<-1}
    if(length(ab.val)<=1){ab.val<-rep(ab.val,length(max))}

    if(length(ab.val)>0){
    for(i in 1:length(ab.val)){
    if(is.na(ab.val[i])){ab.val[i]<-1}
    }}#make sure that abundance values exist
    
    which(as.numeric(min)<=x)->a
    which(as.numeric(max)>=x)->b
    intersect(a,b)->id

    #sum(ab.val[id], na.rm=TRUE)->abundance
    length(id)->abundance
    return(abundance)
    }

length(x)->n
rep(NA,n)->tmp
#if(!(is.null(max) & is.null(min))){
for(i in 1:n){
abdistr(x[i], table=table, ab.val=ab.val)*w[i]->tmp[i]}
#}else{rep(NA,n)->tmp}

if(smooth>0){tmp<-rmeana(y0=tmp,x0=x, plusminus=smooth)}

return(tmp)
}
##


##Function pdb.union
#'Form the union of two occurrence data.frames or remove duplicates from occurrence data.frame. Useful if parts of a clade are not included in the downloaded dataset and need to be added separately.
#'
#' @param x Concatenated occurrence data.frames to be merged
#' @param id_col Vector or column of x containing id to be used for determining which values contain occurrence numbers to be used for matching entries
#' @return A data.frame() containing the first entry for each unique occurrence to be represented in x.
#' @export pdb.union
#' @examples
#' data(archosauria)
#' pdb.union(rbind(archosauria$Ankylosauria, archosauria$Stegosauria))->Eurypoda

pdb.union<-function(x,id_col=x$occurrence_no){

nrow(x)->n_o
for(i in 1:length(x)){

which(id_col == id_col[i])->tmp

if(length(tmp)>1){
x[-tmp[2:length(tmp)],]->x
}

}
message(paste("Dropping", n_o-nrow(x),"duplicate entries"))

return(x)
}
##




##Function pdb.diff
#'Subtract one occurrence data.frame from another, for disentangling overlapping taxonomies or quantifying stem-lineage diversity.
#'
#' @param x Occurrence data from which to subtract.
#' @param subtract Occurrence data frame or vector of occurrence numbers to subtract from x
#' @param id_col Vector or column of x containing id to be used for determining which values are also found in subtract or subtract$occurrence_no
#' @return A data.frame() containing the difference between the two occurrence datasets, i.e. all entries that are in x but not in subtract.
#' @export pdb.diff
#' @examples
#' data(archosauria)
#' pdb.union(rbind(archosauria$Ankylosauria, archosauria$Stegosauria))->Eurypoda
#' pdb.diff(Eurypoda, subtract=archosauria$Stegosauria)

pdb.diff<-function(x,subtract,id_col=x$occurrence_no){

if(is.data.frame(subtract)){
drop<-which(id_col %in% subtract$occurrence_no)
}else{
drop<-which(id_col %in% subtract)
}
if(length(drop>0)){
message(paste("Dropping", length(drop),"rows from occurrence data frame"))
x[-drop,]}else{x}
}
##


##Function stax.sel
#'Extract subsets of an occurrence data.frame.
#'
#' @param taxa A vector containing subtaxa (or any other entries matching entries of rank) to be returned
#' @param rank Vector or column of x in which to look for entries matching taxa. defaults to x$class, for selecting class-level subtaxa from large datasets (only works if pdb(...,full=TRUE))
#' @param x Optional occurrence data.frame. If set, a data.frame with the selected entries will be returned.
#' @return If is.null(x) (default), a vector giving the indices of values matching taxa in rank. Otherwise, an occurrence data.frame() containing only the selected taxa or values.
#' @export stax.sel
#' @examples
#' data(archosauria)
#' archosauria$Stegosauria->stegos
#' stax.sel(c("Stegosaurus"), rank=stegos$genus,x=stegos)->Stegosaurus

stax.sel<-function(taxa, rank=x$class, x=NULL){
tmp<-numeric()

for(i in 1:length(taxa)){
which(rank==taxa[i])->ids
c(tmp, ids)->tmp
}

if(!is.null(x)){
return(x[tmp,])
}else{
return(tmp)}

}
##

##Function occ.cleanup
#' Clean up occurrence dataset by removing commonly used character combinations in the identified name that will result in different factor levels for the same taxon.
#'
#' @param x A occurrence data.frame or character vector containing the variable to clean up (defaults to x$tna)
#' @param remove Which values to remove. If NULL, a default set of commonly occurring character combinations is used ("n. gen.", "n. sp.", "cf.","aff.", punctuation, as well as double, leading and ending spaces). If user-defined, remove needs to be formatted as a character vector with the values to be removed as names, i.e. in the format of c("remove_this" = "", "removethistoo"="")
#' @param return.df A logical indicating whether to return the entire data.frame (if TRUE) or just the column of taxonomic names.
#' @return A character vector containing the cleaned up taxonomic names or a dataframe with cleaned-up tna column (if return.df==TRUE).
#' @importFrom stringr str_replace_all
#' @export occ.cleanup
#' @examples
#' data(archosauria)
#' occ.cleanup(archosauria$Stegosauria)->archosauria$Stegosauria

occ.cleanup<-function(x,remove=NULL,return.df=FALSE){
if(is.null(remove)){
remove<-c("aff. "="","n. gen. "="","cf. "=""," $"="", "^ "="", "n. sp. "="","[[:punct:]]"="", "  "=" ")
}

if(is.data.frame(x)){
length(levels(factor(x$tna)))->lev
stringr::str_replace_all(x$tna, remove)->out

if(return.df==TRUE){
x$tna<-out
}

}else{
length(levels(factor(x)))->lev
stringr::str_replace_all(x, remove)->out
}

message(paste(lev, "factor levels reduced down to", length(levels(factor(out)))))

if(return.df==TRUE & is.data.frame(x)){return(x)}else{
return(out)}

}
##


##Function divdistr_int
#'Count number of taxon records overlapping a specific time interval.
#'
#' @param x A numeric vector of length 2 specifying the start and end (in ma) of the time interval in question.
#' @param table Taxon-range table to use
#' @param ids Logical whether to return ids of entries in taxon-range table (defaults to FALSE) or their number
#' @return A single numeric giving the number of entries in table overlapping the specified interval, or a numeric vector giving their indices.
#' @param max Vector or column containing the maximum age of each entry in the taxon-range table. Defaults to table$max
#' @param min Vector or column containing the minimum age of each entry in the taxon-range table. Defaults to table$min
#' @export divdistr_int
#' @examples
#' data(archosauria)
#' divdistr_int(x=c(201,220), table=archosauria$sptab_Coelophysoidea)

divdistr_int<-function(x,table=NULL, ids=FALSE, max=table$max, min=table$min){#here x needs to be a vector of length 2 containing the minimum and maximum ages defining the interval
which(min<=max(x))->a
which(min>=min(x))->b

which(max<=max(x))->c
which(max>=min(x))->d

intersect(a,b)->id1#intersection contains those entries that have a minimum smaller than the maximum but larger than the minimum
intersect(c,d)->id2#intersection contains those entries that have a maximum smaller than the maximum but larger than the minimum

union(id1,id2)->id#union of both intersections, giving all taxa that have any temporal overlap with the selected interval

length(id)->length
if(ids==TRUE){return(id)#setting ids=TRUE returns ids of records overlapping interval
}else{
return(length)}#standard setting ids=FALSE returns number of records
}
##


##Function pdb.autodiv
#'A wrapper around pdb(), occ.cleanup() and mk.sptab() to automatically download and clean occurrence data from the paleobiology database and build species-level taxon-range tables for multiple taxa in one step.
#'
#' @param taxa Either a character vector of valid taxonomic names, or an object of class "phylo" whose tip.labels to use instead.
#' @param cleanup Logical indicating whether to apply occ.cleanup() to occurrence data after download (defaults to TRUE)
#' @param interval Stratigraphic interval for which to download data (defaults to NULL, which downloads data for all intervals)
#' @return A list() object containing occurrence data (saved under the taxon names given) and species-level taxon-range tables (saved with the prefix "sptab_" before the taxon names).
#' @export pdb.autodiv
#' @examples
#' pdb.autodiv("Coelophysoidea")->coelo

pdb.autodiv<-function(taxa,cleanup=TRUE,interval=NULL){
occ<-list()
if(inherits(taxa, "phylo")){
taxa$tip.label->treetips
}else{taxa->treetips}

#download and cleanup
for(i in 1:length(treetips)){

if(is.null(interval)){
pdb(treetips[i])->occ[[i]]
}else{
pdb(treetips[i],interval=interval)->occ[[i]]
}


if(cleanup==TRUE & is.data.frame(occ[[i]])){
occ.cleanup(occ[[i]])->occ[[i]]$tna}

names(occ)[i]<-treetips[i]
}

#build species tables
for(i in 1:length(treetips)){

if(is.data.frame(occ[[treetips[i]]])){

mk.sptab(occ[[treetips[i]]],tax=treetips[i])->occ[[length(treetips)+i]]

names(occ)[length(treetips)+i]<-paste0("sptab_", treetips[i])

}else{#is no data frame is found, do not build species table
message("No occurrence data.frame() found for ", treetips[i], ". Proceeding without it.")}
}


return(occ)
}
##




##Function tree.ages
#'Automatically build matrix for time-calibration of phylogenetic trees using occurrence data
#'
#' @param phylo0 (Optional) Object of class=="phylo" from which to draw taxa to include in calibration matrix
#' @param data Optional list()-object containing either taxon-range tables or occurrence datasets for all taxa. If NULL, data will be automatically downloaded via the pdb()-function
#' @param taxa Taxa to include in calibration matrix, defaults to phylo0$tip.label
#' @return A two-column matrix containing earliest and latest occurrences for each taxon in taxa, with taxon names as row names
#' @importFrom utils read.csv
#' @export tree.ages
#' @examples
#' data(archosauria)
#' data(tree_archosauria)
#' tree.ages(tree_archosauria,data=archosauria)->ages

tree.ages<-function(phylo0=NULL, data=NULL, taxa=phylo0$tip.label){
FAD<-numeric(length(taxa))
LAD<-numeric(length(taxa))
e<-0

if(is.null(data)){#if no list() object given, look up data on the paleobiology database server
for(i in 1:length(taxa)){
pdb(taxa[i])->data_
if(is.data.frame(data_)){
data_[,c("eag","lag")]->data_
#find and save minimum and maximum recorded taxon ages
max(data_)->FAD[i]
min(data_)->LAD[i]
}else{#if data contains an error or warning instead of an occurrence table
e<-1
NA->FAD[i]
NA->LAD[i]
}
}}else{#try to find taxa in list() object
for(i in 1:length(taxa)){

data[[paste0("sptab_",taxa[i])]][,c("max","min")]->data_#first see if there are species tables

if(is.null(data_) & !is.null(data[[taxa[i]]])){#if species tables were not found, look for occurrence tables
data[[taxa[i]]][,c("eag","lag")]->data_
}

if(is.null(data_)){
data_<-c(NA,NA)
e<-1
}
#find and save minimum and maximum recorded taxon ages
max(data_,na.rm=TRUE)->FAD[i]
min(data_,na.rm=TRUE)->LAD[i]

}
}

#build matrix, then return
cbind(FAD, LAD)->ages
colnames(ages)<-c("FAD", "LAD")
rownames(ages)<-taxa

if(e==1){
warning("Some occurrence tables could not be found, corresponding collumns contain NAs.")
}

return(ages)
}
##

##Function convert.sptab
#'Convert geological ages in taxon-range tables as constructed by mk.sptab() for plotting alongside a time-calibrated phylogeny.
#'
#' @param sptab Taxon-range table to convert
#' @param tree Optional phylogenetic tree to draw root.time from
#' @param root.time Root time of the tree, used for converting ages
#' @return A data.frame() object in the format of the original taxon-range table, but with geological ages converted for plotting alongside the the phylogenetic tree.
#' @export convert.sptab
#' @examples
#' data(archosauria)
#' data(tree_archosauria)
#' convert.sptab(archosauria$sptab_Coelophysoidea,tree_archosauria)

convert.sptab<-function(sptab,tree=NULL,root.time=tree$root.time){
#which(sptab$max>root.time)->drop
#if(length(drop)>0){
#sptab[-drop]->sptab}
sptab->sptab_
sptab_$max<--1*(sptab$min-root.time)
sptab_$min<--1*(sptab$max-root.time)
sptab_$ma<--1*(sptab$max-root.time)
return(sptab_)
}
##



##Function phylo.spindles
#'Plots a phylogenetic tree with spindle-diagrams, optimized for showing taxonomic diversity.
#'
#' @param phylo0 A time-calibrated phylogenetic tree to plot with spindle diagrams, or a character vector of taxonomic names for which to plot spindle diagrams.
#' @param occ Either a list()-object containing taxon-range tables for plotting diversity, or a matrix() or data.frame()-object that contains numerical plotting statistics. If the latter is provided, the default use of divdistr_() is overridden and the function will look for a column named "x" and columns matching the phylogeny tip.labels to plot the spindles.
#' @param stat Plotting statistic to be passed on to viol(). Defaults to use divdistr_().
#' @param prefix Prefix for taxon-range tables in occ. Defaults to "sptab_"
#' @param ages Optional matrix with lower and upper age limits for each spindle, formatted like the output of tree.ages() (most commonly the same calibration matrix used to time-calibrate the tree)
#' @param xlimits Limits for plotting the on the x axis.
#' @param res Temporal resolution of diversity estimation (if occ is a matrix or data.frame containing plotting statistics, this is ignored)
#' @param weights Weights for diversity estimation. Must have the same length as the range of xlimits divided by res. For details, see divdistr_()
#' @param dscale Scale value of the spindles on the y axis. Should be adjusted manually to optimize visibility of results.
#' @param col Color to use for the border of the plotted spindles
#' @param fill Color to use for the fill of the plotted spindles. Defaults to col.
#' @param lwd Line width for the plotted spindles.
#' @param lty Line type for the plotted spindles.
#' @param cex.txt Adjustment for tip label text size
#' @param col.txt tip label text color, defaults to be same as col, but with no transparency
#' @param axis Logical indicating whether to plot (temporal) x axis (defaults to TRUE)
#' @param labels Logical indicating whether to plot tip labels of phylogeny (defaults to TRUE)
#' @param txt.y y axis alignment of tip labels
#' @param txt.x x coordinates for plotting tip labels. Can be a single value applicable to all labels, or a vector of the same length as phylo0$tip.label
#' @param add Logical indicating whether to add to an existing plot, in which case only the spindles are plotted on top of an existing phylogeny, or not, in which case the phylogeny is plotted along with the spindles.
#' @param tbmar Top and bottom margin around the plot. Numeric of either length 1 or 2
#' @param smooth Smoothing parameter to be passed on to divdistr_()
#' @return A plotted phylogeny with spindle diagrams plotted at each of its terminal branches.
#' @details
#' The phylo.spindles() function allows the plotting of a phylogeny with spindle diagrams at each of its terminal branches. Various data can be represented (e.g. disparity, abundance, various diversity measures, such as those output by the divDyn package, etc.) depending on the settings for occ and stat, but the function is optimized to plot the results of divdistr_() and does so by default.
#' If another function is used as an argument to stat, it has to be able to take the sequence resulting from xlimits and res as its first, and occ as its 'table' argument and return a vector of the same length as range(xlimits)/res to be plotted. If occ is a list() object containing multiple dataframes, occurrence datasets or taxon range tables are automatically converted to work with abdistr_() or divdistr_() respectively (if the plot contains a phylogeny). If occ is a matrix or data.frame, the x values must already be converted (e.g. using tsconv()) to match the phylogeny.
#' @importFrom grDevices dev.cur
#' @export phylo.spindles
#' @examples
#' data(archosauria)
#' data(tree_archosauria)
#' data(ages_archosauria)
#' data(diversity_table)
#' phylo.spindles(tree_archosauria,occ=archosauria,dscale=0.005,ages=ages_archosauria,txt.x=66)
#' phylo.spindles(tree_archosauria,occ=diversity_table,dscale=0.005,ages=ages_archosauria,txt.x=66)

phylo.spindles<-function(phylo0, occ, stat=divdistr_, prefix="sptab_", ages=NULL, xlimits=NULL, res=1, weights=1, dscale=0.002, col=add.alpha("black"), fill=col,lwd=1, lty=1, cex.txt=1,col.txt=add.alpha(col,1), axis=TRUE, labels=TRUE, txt.y=0.5,txt.x=NULL, add=FALSE,tbmar=0.2,smooth=0){

if(length(tbmar)==1){tbmar<-rep(tbmar,2)}#if only one value is given for tbmar, duplicate it. Otherwise, first value is bottom, second top
if(inherits(phylo0,"phylo")){#setting for phylogenetic tree
taxsel<-phylo0$tip.label
txt.x<-phylo0$root.time-txt.x
if(is.null(xlimits)){
xlimits<-c(round(phylo0$root.time)-1,0)}

}else if(is.character(phylo0)){taxsel<-phylo0#settings for taxonomic list
if(!is.null(ages) & is.null(xlimits)){
xlimits<-rev(range(ages))}

}else{stop("phylo0 must be either a phylogenetic tree of a character vector containing taxon names.")}

if(is.null(txt.x)){txt.x<-mean(xlimits)}



##generate base plot
if(add==FALSE){

if(inherits(phylo0,"phylo")){
ape::plot.phylo(phylo0,x.lim=-1*(xlimits-phylo0$root.time),align.tip.label=2, label.offset=50,show.tip.label=FALSE, y.lim=c(1-tbmar[1],length(phylo0$tip.label)+tbmar[2]))->plot1

}else{
plot(NULL, xlim=xlimits,ylim=c(1-tbmar[1], length(taxsel)+tbmar[2]), xlab="",ylab="",axes=FALSE)
plot1<-NULL
}
}else{#if add==FALSE
if(dev.cur()==1){stop("ERROR: No open plotting device to add to")}

plot1<-tryCatch({get("last_plot.phylo", envir = ape::.PlotPhyloEnv)}, error=function(e){return(NULL)})

}

if(is.null(plot1)){
plot1$x.lim<-xlimits
}

#colors
col->col_
fill->fill_
col.txt->col.txt_

#repeat weights
if(length(weights)==1){
weights<-rep(weights,length(seq(min(plot1$x.lim),max(plot1$x.lim),abs(res))))
}

##loop through taxa/tip.labels
for(i in 1:length(taxsel)){
##set spindle limits
if(!is.null(ages)){#if age data is provided

    if(taxsel[i] %in% rownames(ages)){#check if tiplabel can be found in rownames
    cutoff<-as.numeric(ages[rownames(ages)==taxsel[i],])
    }else{
    warning(paste0(taxsel[i], " not found in rownames(ages). Please make sure that rownames in age data match taxa in phylogeny!"))
        if(nrow(ages)==length(taxsel)){#contingency if rownames cannot be matched, but row numbers can:
        cutoff<-as.numeric(ages[i,])
        }else{
        cutoff<-range(eval(parse(text=paste0("occ$",prefix,taxsel[i]))), phylo0[,2:3])}
        }
}else{#use range of data if no validly formatted ages are provided
cutoff<-range(eval(parse(text=paste0("occ$",prefix,taxsel[i]))), phylo0[,c("max","min")])###XXX
}

if(inherits(phylo0,"phylo")){phylo0$root.time-cutoff->cutoff}
#end spindle limits


##vary colors, if desired
if(length(col_)==length(taxsel)){
col<-col_[i]
}
if(length(fill_)==length(taxsel)){
fill<-fill_[i]
}
if(length(col.txt_)==length(taxsel)){
col.txt<-col.txt_[i]
}

#set weights
if(is.matrix(weights)){
w<-weights[,i]
}else if(length(weights)==length(seq(min(plot1$x.lim),max(plot1$x.lim),abs(res)))){
w<-weights
}

if(length(w)!=length(seq(min(plot1$x.lim),max(plot1$x.lim),abs(res)))){stop("weights vector must have the same length as time interval/resolution")}

#prepare data for plotting

if(!is.matrix(occ) & !is.data.frame(occ)){

itable<-occ[[paste0(prefix,taxsel[i])]]

if(is.null(itable$max) & !is.null(itable$eag)){
itable$max<-itable$eag
}
if(is.null(itable$min) & !is.null(itable$lag)){
itable$min<-itable$lag
}
if(inherits(phylo0,"phylo")){
convert.sptab(itable,phylo0)->itable
itable$eag<-itable$max
itable$lag<-itable$min

plotx<-seq(min(plot1$x.lim),max(plot1$x.lim),abs(res))
}else{
if(plot1$x.lim[1]>plot1$x.lim[2] & res>0){res<--res}
plotx<-seq(plot1$x.lim[1],plot1$x.lim[2],res)}#set sequence of x values at which to plot, depending on plotting direction
#plot spindles
viol(plotx,pos=i, stat=stat, table=itable, smooth=smooth, dscale=dscale, col=col, fill=fill, lwd=lwd,lty=lty,cutoff=cutoff,w=w)


}else if(is.matrix(occ) | is.data.frame(occ)){#if instead of a list object, a dataframe is given giving x and diversity values to plot (can also be co-opted to plot any other values, e.g. disparity
if(!("x" %in% colnames(occ))){stop("if occ is a data.frame() or matrix(), it needs to have a column giving x values for plotting with column name == x")
}
if(taxsel[i] %in% colnames(occ)){

viol(occ[,"x"], pos=i, stat=occ[,taxsel[i]], dscale=dscale, col=col, fill=fill, lwd=lwd,lty=lty, cutoff=cutoff, w=w)
}

}

#convert coordinates if phylogeny is being plotted
if(inherits(phylo0,"phylo")){

}

if(labels==TRUE){#add labels
if(length(txt.y)==1){txt.y<-rep(txt.y, length(taxsel))}

if(length(txt.x)>1){#if a vector of x values for labels is provided
    if(length(which(names(txt.x)==taxsel[i]))==1){
    which(names(txt.x)==taxsel[i])->j
    text(x=txt.x[j],y=i,adj=c(0,txt.y[i]), taxsel[i], cex=cex.txt,col=col.txt)
    }else{
    text(x=txt.x[i],y=i,adj=c(0,txt.y[i]), taxsel[i], cex=cex.txt,col=col.txt)}
}else{
text(x=txt.x,y=i,adj=c(0,txt.y[i]), taxsel[i], cex=cex.txt,col=col.txt)}
}

}#end loop


if(axis==TRUE){#add time axis
if(inherits(phylo0, "phylo")){
ticks<-seq(round(min(c(max(xlimits),phylo0$root.time))/10)*10,round(min(xlimits)/10)*10,-25)

axis(1,at=1-(ticks-phylo0$root.time), lab=ticks)}else{axis(1)}

}

}
##



##Function div.gg
#'Make a data.frame() that can be used to plot diversity data with density plots, e.g. in ggplot2
#'
#' @param data list()-object containing taxon-range tables
#' @param taxa Selection of taxa to include
#' @param agerange Range of geological ages to include in data.frame()
#' @param precision_ma Size of intervals (in ma) at which to calculate diversity within the age range.
#' @param prefix Prefix under which to find taxon-range tables in data
#' @return A data.frame() with two columns: ma, for the numerical age, and tax, for the taxon. 
#' @details
#' Each taxon receives one entry per subtaxon (e.g. species) occurring for each time interval at which it occurs. The number of entries per taxon at any given point is thus proportional to the diversity of the taxon, and can be used to trick density functions (e.g. hist(), density()) into plotting diversity diagrams of various types. This is most useful when using ggplot2::geom_violin(), geom_histogram() or geom_density() functions. 
#' A simpler alternative to achieve a similar result would be to use the taxon-range-tables directly with these functions. However, this will lead to a relative underestimate of diversity for taxa with long-lived subtaxa, since each subtaxon will only be counted once. The div.gg()-function circumvents this problem by representing each taxon for each time interval in which it occurs, i.e. the relative number of entries in the returned data.frame will be proportional to the relative number of taxa with ranges overlapping each point in time.
#' @export div.gg
#' @examples
#' data(archosauria)
#' div.gg(archosauria, taxa=c("Pterosauria","Aves"), agerange=c(252,0),precision_ma=1)->flyers
#' library(ggplot2)
#' ggplot(data=flyers, aes(x=tax, y=ma))+ylim(252,0)+geom_violin(scale="count")
#' ggplot(data=flyers, aes(col=tax, x=ma))+xlim(252,0)+geom_density(adjust=0.5)

div.gg<-function(data, taxa, agerange=c(252,66), precision_ma=1,prefix="sptab_"){#occ needs to be a list()-object with mk.sptab-output for relevant subtaxa saved as occ$sptab_taxonname)
occ<-data
ma<-numeric()
tax<-character()#empty vectors to append to
agerange<-seq(max(agerange),min(agerange),-precision_ma)#make a sequence out of agerange based on precision

for(i in 1:length(taxa)){
st<-eval(parse(text=paste0("occ$",prefix,taxa[i])))#get the value taxon-range table i and save to st
taxonfun<-divdistr_(agerange, table=st)#make diversity distribution for taxon

ma<-c(ma,c(rep(agerange, taxonfun)))#repeat each value for agerange by the estimated diversity at that time

tax<-c(tax,rep(taxa[i], sum(taxonfun)))#repeat taxon names according to the sum of their diversity values
}#repeat for each taxon in taxa/sptab_taxon in occ
data.frame(ma=ma, tax=tax)->dd
return(dd)
}
##



##Function ab.gg
#'Make a data.frame() that can be used to plot diversity data with density plots, e.g. in ggplot2
#'
#' @param data list()-object containing occurrence data.frames or single occurrence data.frame()
#' @param taxa Selection of taxa to include. If NULL, then abundance is tabulated for each unique factor level of data$tna
#' @param agerange Range of geological ages to include in data.frame()
#' @param precision_ma Size of intervals (in ma) at which to calculate diversity within the age range.
#' @return A data.frame() with two columns: ma, for the numerical age, and tax, for the taxon. 
#' @details
#' Each taxon receives one entry per occurrence per time interval. The number of entries per taxon at any given point is thus proportional to the abundance of the taxon in the fossil record, and can be used for plotting with frequency- or density-based functions (e.g. hist(), ggplot2::geom_violin(), etc.). Note that using age values in the original occurrence table instead of this function will often be fully sufficient if the number of occurrences is considered an adequate proxy for abundance. However, instead using the ab.gg() and thus visualizing the results of the abdistr_() function has the benefit of the ability to account for a column of abundance values within the occurrence dataset, if available.
#' @export ab.gg
#' @examples
#' data(archosauria)
#' ab.gg(data=archosauria, taxa=c("Ankylosauria","Stegosauria"))->thyreophora
#' library(ggplot2)
#' ggplot(data=thyreophora, aes(x=tax, y=ma, col=tax))+ylim(252,0)+geom_violin(scale="count")

ab.gg<-function(data, taxa=NULL, agerange=c(252,66), precision_ma=1){
ma<-numeric()
tax<-character()#just empty vectors to append our values to
agerange<-seq(min(agerange),max(agerange),precision_ma)#this just makes a sequence from the given age interval

###for individual species
if(is.data.frame(data) & is.null(taxa)){
levels(factor(data$tna))->lev
for(i in 1:length(lev)){

st<-data[data$tna==lev[i],]#get the table for each species and save it as st.


###
abdistr_(agerange, table=st)->abundance#this computes the abundance for the agerange

ma<-c(ma,c(rep(agerange, abundance)))#this repeats each value in the age range as often as there are occurrences or specimens from that time

tax<-c(tax,rep(lev[i], sum(abundance)))#this simply repeats the name of the species as often as there are specimens
}
}else{
for(i in 1:length(taxa)){
if(is.data.frame(data)){st<-data}else if(is.list(data) & !is.null(taxa)){
st<-data[[taxa[i]]]#get the value of each taxon and save it as st.
}else{stop("data must be a data.frame() or a list()-object containing data.frames")}


abdistr_(agerange, table=st)->abundance#this computes the abundance for the agerange

ma<-c(ma,c(rep(agerange, abundance)))#repeats each value in the age range as often as there are occurrences or specimens from that time

tax<-c(tax,rep(taxa[i], sum(abundance)))#repeats the name of the taxon as often as there are specimens
}}



data.frame(ma=ma, tax=tax)->dd#join both columns into dataframe
return(dd)
}
##
