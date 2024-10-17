## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(paleoDiv)

## ----eval=FALSE---------------------------------------------------------------
#  pdb("Stegosauria")->Stegosauria

## ----echo=FALSE---------------------------------------------------------------
data(archosauria) #load the same dataset, but from the data distributed with the package, in case the server is down
archosauria$Stegosauria->Stegosauria
Stegosauria$identified_name->Stegosauria$tna

## -----------------------------------------------------------------------------
knitr::kable(head(Stegosauria))

## -----------------------------------------------------------------------------
occ.cleanup(Stegosauria)->Stegosauria$tna

## ----message=FALSE------------------------------------------------------------
mk.sptab(Stegosauria)->sptab_Stegosauria

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(sptab_Stegosauria))

## ----eval=FALSE---------------------------------------------------------------
#  pdb.autodiv(c("Stegosauria","Ankylosauria"))->Thyreophora

## -----------------------------------------------------------------------------
divdistr_(150,table=sptab_Stegosauria)

## -----------------------------------------------------------------------------
divdistr_(c(170:120),table=sptab_Stegosauria)

## ----fig.height=5, fig.width=7------------------------------------------------
curve(divdistr_(x,sptab_Stegosauria), xlim=c(200,100),ylim=c(-5,35), lwd=2,xlab="Age [ma]",ylab="Species Diversity Estimate")
#to add a geological timescale, we can use ts.stages() and ts.periods():
ts.stages(ylim=c(-6,-1),alpha=0.3,border=add.alpha("grey",0.3))
ts.periods(ylim=c(-6,-1),alpha=0.0)

## ----fig.height=4, fig.width=7------------------------------------------------
viol(c(100:200), pos=0, stat=divdistr_, table=sptab_Stegosauria, lim=c(200,100),add=F,ylab="Species Diversity Estimate",xlab="Age [ma]")

## ----fig.height=5, fig.width=7------------------------------------------------
curve(divdistr_(x,sptab_Stegosauria), xlim=c(200,100),ylim=c(-5,35), lwd=2,xlab="Age [ma]",ylab="Species Diversity Estimate")
#to add a geological timescale, we can use ts.stages() and ts.periods():
ts.stages(ylim=c(-6,-1),alpha=0.3,border=add.alpha("grey",0.3))
ts.periods(ylim=c(-6,-1),alpha=0.0)

curve(abdistr_(x,Stegosauria,ab.val=1)/4, xlim=c(200,100),ylim=c(-5,35),col="red", lwd=2,lty=2,add=T)
axis(4,at=seq(0,30,5), lab=seq(0,30,5)*4, col.axis="red")

## ----eval=FALSE---------------------------------------------------------------
#  pdb("Stegosauria", what="colls")->Stegosauria_colls

## ----echo=FALSE---------------------------------------------------------------
data(archosauria) #load the same dataset, but from the data distributed with the package, in case the server is down
archosauria$Stegosauria_colls->Stegosauria_colls

## ----fig.height=5, fig.width=7------------------------------------------------
curve(divdistr_(x,sptab_Stegosauria), xlim=c(200,100),ylim=c(-5,35), lwd=2,xlab="Age [ma]",ylab="Species Diversity Estimate")
#to add a geological timescale, we can use ts.stages() and ts.periods():
ts.stages(ylim=c(-6,-1),alpha=0.3,border=add.alpha("grey",0.3))
ts.periods(ylim=c(-6,-1),alpha=0.0)

#now plot the number of collections alongside the diversity curve
curve(abdistr_(x,Stegosauria_colls,ab.val=1)/4, xlim=c(200,100),ylim=c(-5,35),col="blue", lwd=2,lty=2,add=T)
axis(4,at=seq(0,30,5), lab=seq(0,30,5)*4, col.axis="blue")

## -----------------------------------------------------------------------------
data(archosauria)
data(tree_archosauria)
data(ages_archosauria)

## ----fig.height=5, fig.width=7------------------------------------------------
ape::plot.phylo(tree_archosauria)
ts.stages(tree_archosauria,alpha=0.8)
ts.periods(tree_archosauria, names=T, alpha=0)

## ----fig.height=7, fig.width=7------------------------------------------------
phylo.spindles(phylo0=tree_archosauria,occ=archosauria,col=add.alpha(ggcol(13)),ages=ages_archosauria,txt.y=.5, dscale=0.005,xlim=c(260,0),axis=F,tbmar=c(1.5,.5),txt.x=c(66,ages_archosauria[2:12,"LAD"],66))
#add a timescale
ts.stages(tree_archosauria, ylim=c(-1,0),alpha=0.8)
ts.periods(tree_archosauria, names=T, ylim=c(-1,0),alpha=0)

#add an x axis with custom tick positions:
axis(1,at=tsconv(seq(300,-50,-50),tree_archosauria), lab=seq(300,-50,-50), cex=0.75,col="grey30",col.lab="grey30")
#add a short y axis serving as a scale bar
axis(2, at=c(500,1000)*0.005, lab=c(0,500))

#add vertical lines marking major mass extinctions.
abline(v=tsconv(c(252,201.3,66),tree_archosauria),lwd=2, col=add.alpha("black",0.3))

mtext(side=3, cex=1.2,"paleoDiv::divdistr_()",col="darkgrey")

#we can manually add spindles anywhere using viol(), e.g. to plot the diversity of important subtaxa, in this case that of birds:
viol(x=c(260:0),stat=divdistr_, pos=13, table=convert.sptab(archosauria$sptab_Aves,tree_archosauria),dscale=0.005, cutoff=tsconv(c(165,0),tree_archosauria),fill=add.alpha("grey40"), col=add.alpha("grey40"))

## ----fig.height=7, fig.width=7------------------------------------------------
data(diversity_table)

phylo.spindles(tree_archosauria,occ=diversity_table,ages=ages_archosauria,txt.y=.5,xlim=c(260,0),axis=F,dscale=0.01,col=add.alpha(ggcol(13)),tbmar=c(1.5,0.5),txt.x=c(66,ages_archosauria[2:12,"LAD"],66))

#add a timescale
ts.stages(tree_archosauria, ylim=c(-1,0),alpha=0.8)
ts.periods(tree_archosauria, names=T, ylim=c(-1,0),alpha=0)

#add an x axis with custom tick positions:
axis(1,at=tsconv(seq(300,-50,-50),tree_archosauria), lab=seq(300,-50,-50), cex=0.75,col="grey30",col.lab="grey30")
#add a short y axis serving as a scale bar
axis(2, at=c(500,1000)*0.01, lab=c(0,500))

#add vertical lines marking major mass extinctions.
abline(v=tsconv(c(252,201.3,66),tree_archosauria),lwd=2, col=add.alpha("black",0.3))

mtext(side=3, cex=1.2,"divDyn::divDyn()$divRT",col="darkgrey")


## ----fig.height=7, fig.width=7------------------------------------------------
phylo.spindles(phylo0=tree_archosauria$tip.label,occ=archosauria,col=add.alpha(ggcol(13)),ages=ages_archosauria,txt.y=.5, dscale=0.005,xlim=c(260,0),axis=F,tbmar=c(1.5,.5),txt.x=c(66,ages_archosauria[2:12,"LAD"],66))
#add a timescale
ts.stages(ylim=c(-1,0),alpha=0.8)
ts.periods(names=T, ylim=c(-1,0),alpha=0)

#add an x axis with custom tick positions:
axis(1,at=seq(300,-50,-50), cex=0.75,col="grey30",col.lab="grey30")
#add a short y axis serving as a scale bar
axis(2, at=c(500,1000)*0.005, lab=c(0,500))

#add vertical lines marking major mass extinctions.
abline(v=c(252,201.3,66),lwd=2, col=add.alpha("black",0.3))

mtext(side=3, cex=1.2,"Phylo.spindles without Phylogeny",col="darkgrey")

## ----fig.height=7, fig.width=7------------------------------------------------
ape::plot.phylo(tree_archosauria)

phylo.spindles(phylo0=tree_archosauria,occ=archosauria,col=add.alpha(ggcol(13)),ages=ages_archosauria,txt.y=.5, dscale=0.005,xlim=c(260,0),axis=F,tbmar=c(1.5,.5),txt.x=c(66,ages_archosauria[2:12,"LAD"],66),add=T)

