
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntensiveACiFit

<!-- badges: start -->
<!-- badges: end -->

The goal of IntensiveACiFit is to fit intensive data mentioned in Zhou
et al (2019). All the codes are from
[](https://github.com/zhouhaoran06/C4-Parameter-Estimation) and revised
for use with LI-6800 RACiR data or ACi data measured by LI-6800 use the
method advised by Zhou et al (2019). If you use this package, please
cite Zhou et al (2019) first:

> Zhou, Haoran, Erol Akçay, 和 Brent R. Helliker. 2019. Estimating C4
> photosynthesis parameters by fitting intensive A/Ci curves.
> Photosynthesis Research 141 (2): 181–94.
> <https://doi.org/10.1007/s11120-019-00619-8>.

## Installation

You can install the development version of IntensiveACiFit like so:

``` r
remotes::install_github('zhujiedong/IntensiveACiFit')
# or
remotes::install_git('https://gitee.com/zhu_jie_dong/IntensiveACiFit')
```

## Example

This is a basic example which shows you how to solve a common problem.

<div class="callout-note">

Please note that this is the very begining of this package. I plan to
add plot method to give a quick way to plot the fitted data, but it is
still in plan. The following show some codes that may be helpful to give
a quick view of the fiting results.

</div>

In the extdata file in the package, I included a simulated RACiR data
use `plantecophys::ACiC4` (as currently I do not have RACiR data
measured in C4 plant).

The format of the data should be LI-6800 data without changing the
header of the data as the followings:

elapsed\| E\| Emm\| A\| \|

\|——-:\|———:\|——–:\|———:\|—-\|\| 0\| 0.0018594\| 1.859397\| 5.589868\|
50.\| \| 2\| 0.0018582\| 1.858232\| 7.963561\| 50.\| \| 4\| 0.0018556\|
1.855645\| 10.675401\| 50.\| \| 6\| 0.0018550\| 1.854993\| 13.590991\|
50.\| \| 8\| 0.0018522\| 1.852195\| 15.891795\| 50.\|

You can also refer to the example data.

Please refer to Zhou et al (2019) for the details of the function
parameters.

``` r
# read the LI-6800 data
aci = read.csv('inst/extdata/sim-data-utf8.csv')

#-------------- C4EstimateWithCA -------------------#
ca_c4estimate = C4EstimateWithCA(aci, alpha1 = 0.15, x=0.4, 
    CaBreakL = 10, CaBreakH = 45, startp = c(45, 300, 3, 20, 120, 100))
str(ca_c4estimate)

A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(ca_c4estimate$limitation_stage[1], use.names=FALSE)
RcPr = unlist(ca_c4estimate$limitation_stage[2], use.names=FALSE)
RrPc = unlist(ca_c4estimate$limitation_stage[3], use.names=FALSE)
RrPr = unlist(ca_c4estimate$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

# plot data
plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))


#-------------- C4EstimateWithCAT -------------------#
cat_c4estimate = C4EstimateWithCAT(aci, CaBreakL = 10, CaBreakH = 35)
str(cat_c4estimate)
A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(cat_c4estimate$limitation_stage[1], use.names=FALSE)
RcPr = unlist(cat_c4estimate$limitation_stage[2], use.names=FALSE)
RrPc = unlist(cat_c4estimate$limitation_stage[3], use.names=FALSE)
RrPr = unlist(cat_c4estimate$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))


#-------------- C4EstimateWithoutCA -------------------#
noca_c4estimate = C4EstimateWithoutCA(aci, CaBreakL = 10, CaBreakH = 35)
str(noca_c4estimate)

A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(noca_c4estimate$limitation_stage[1], use.names=FALSE)
RcPr = unlist(noca_c4estimate$limitation_stage[2], use.names=FALSE)
RrPc = unlist(noca_c4estimate$limitation_stage[3], use.names=FALSE)
RrPr = unlist(noca_c4estimate$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))


#-------------- C4EstimateWithoutCAT -------------------#
nocat_c4estimate = C4EstimateWithoutCAT(aci, CaBreakL = 10, CaBreakH = 25)
str(nocat_c4estimate)
A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(nocat_c4estimate$limitation_stage[1], use.names=FALSE)
RcPr = unlist(nocat_c4estimate$limitation_stage[2], use.names=FALSE)
RrPc = unlist(nocat_c4estimate$limitation_stage[3], use.names=FALSE)
RrPr = unlist(nocat_c4estimate$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))

#-------------- C4EstimateWithoutCAYin -------------------#
noca_c4estimate_yin = C4EstimateWithoutCAYin(aci, CaBreakL = 10, CaBreakH = 35)
str(noca_c4estimate_yin)

A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(noca_c4estimate_yin$limitation_stage[1], use.names=FALSE)
RcPr = unlist(noca_c4estimate_yin$limitation_stage[2], use.names=FALSE)
RrPc = unlist(noca_c4estimate_yin$limitation_stage[3], use.names=FALSE)
RrPr = unlist(noca_c4estimate_yin$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))


#-------------- C4EstimateWithoutCAYin -------------------#
nocat_c4estimate_yin = C4EstimateWithoutCAYin(aci, CaBreakL = 10, CaBreakH = 25)
str(nocat_c4estimate_yin)
A.obs = aci$A
Ci.obs = aci$Pci
RcPc = unlist(nocat_c4estimate_yin$limitation_stage[1], use.names=FALSE)
RcPr = unlist(nocat_c4estimate_yin$limitation_stage[2], use.names=FALSE)
RrPc = unlist(nocat_c4estimate_yin$limitation_stage[3], use.names=FALSE)
RrPr = unlist(nocat_c4estimate_yin$limitation_stage[4], use.names=FALSE)

# colors, axis range, and texts used in the plot
cols = palette('Set 3')[3:7]
xrange<-max(Ci.obs)
yrange<-max(A.obs) + max(A.obs) * 0.1
leg.text<-c("Obs A", "Cal RcPc", "Cal RcPr","Cal RrPc","Cal RrPr")

plot(Ci.obs, A.obs, pch =19, col = cols[1], 
        xlim=range(0,xrange),ylim=range(0,yrange), xlab="Ci(Pa)",ylab="A")
lines(Ci.obs[order(Ci.obs)], RcPc[order(Ci.obs)], col=cols[2], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RcPr[order(Ci.obs)], col=cols[3], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPc[order(Ci.obs)], col=cols[4], lwd=4.2, pch = 20)
lines(Ci.obs[order(Ci.obs)], RrPr[order(Ci.obs)], col=cols[5], lwd=4.2, pch = 20)
legend("bottomright",leg.text,col=cols,
        pch=c(20,NA,NA,NA,NA),lty=c(0,1,1,1,1),cex=0.5,lwd=c(0,2,2,2,2))
```
