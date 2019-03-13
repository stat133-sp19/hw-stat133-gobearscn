Workout01
================
Daiqiao Lin

<img src="../images/gsw-shot-chart.png" width="80%" style="display: block; margin: auto;" />

eff shooting by %
-----------------

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
ap<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//shots-data.csv',stringsAsFactors = FALSE)

currys<-ap[ap$player=='Stephen Curry',c('player','shot_made_flag','shot_type')]
csumtotal=sum(table(currys$shot_made_flag))
csumyestotal=sum(table(currys$shot_made_flag[currys$shot_made_flag=='shot_yes']))
ctwototal=currys[currys$shot_type=='2PT Field Goal',]
cthreetotal=currys[currys$shot_type=='3PT Field Goal',]
csum2total=sum(table(ctwototal$shot_type))
csum2yestotal=sum(table(ctwototal$shot_type[ctwototal$shot_made_flag=='shot_yes']))
csum3total=sum(table(cthreetotal$shot_type))
csum3yestotal=sum(table(cthreetotal$shot_type[cthreetotal$shot_made_flag=='shot_yes']))

kevins<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
ksumtotal=sum(table(kevins$shot_made_flag))
ksumyestotal=sum(table(kevins$shot_made_flag[kevins$shot_made_flag=='shot_yes']))
ktwototal=kevins[kevins$shot_type=='2PT Field Goal',]
kthreetotal=kevins[kevins$shot_type=='3PT Field Goal',]
ksum2total=sum(table(ktwototal$shot_type))
ksum2yestotal=sum(table(ktwototal$shot_type[ktwototal$shot_made_flag=='shot_yes']))
ksum3total=sum(table(kthreetotal$shot_type))
ksum3yestotal=sum(table(kthreetotal$shot_type[kthreetotal$shot_made_flag=='shot_yes']))

thompson<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
tsumtotal=sum(table(thompson$shot_made_flag))
tsumyestotal=sum(table(thompson$shot_made_flag[thompson$shot_made_flag=='shot_yes']))
ttwototal=thompson[thompson$shot_type=='2PT Field Goal',]
tthreetotal=thompson[thompson$shot_type=='3PT Field Goal',]
tsum2total=sum(table(ttwototal$shot_type))
tsum2yestotal=sum(table(ttwototal$shot_type[ttwototal$shot_made_flag=='shot_yes']))
tsum3total=sum(table(tthreetotal$shot_type))
tsum3yestotal=sum(table(tthreetotal$shot_type[tthreetotal$shot_made_flag=='shot_yes']))

greens<-ap[ap$player=='Draymond Green',c('player','shot_made_flag','shot_type')]
gsumtotal=sum(table(greens$shot_made_flag))
gsumyestotal=sum(table(greens$shot_made_flag[greens$shot_made_flag=='shot_yes']))
gtwototal=greens[greens$shot_type=='2PT Field Goal',]
gthreetotal=greens[greens$shot_type=='3PT Field Goal',]
gsum2total=sum(table(gtwototal$shot_type))
gsum2yestotal=sum(table(gtwototal$shot_type[gtwototal$shot_made_flag=='shot_yes']))
gsum3total=sum(table(gthreetotal$shot_type))
gsum3yestotal=sum(table(gthreetotal$shot_type[gthreetotal$shot_made_flag=='shot_yes']))


iguodalas<-ap[ap$player=='Andre Iguodala',c('player','shot_made_flag','shot_type')]
isumtotal=sum(table(iguodalas$shot_made_flag))
isumyestotal=sum(table(iguodalas$shot_made_flag[iguodalas$shot_made_flag=='shot_yes']))
itwototal=iguodalas[iguodalas$shot_type=='2PT Field Goal',]
ithreetotal=iguodalas[iguodalas$shot_type=='3PT Field Goal',]
isum2total=sum(table(itwototal$shot_type))
isum2yestotal=sum(table(itwototal$shot_type[itwototal$shot_made_flag=='shot_yes']))
isum3total=sum(table(ithreetotal$shot_type))
isum3yestotal=sum(table(ithreetotal$shot_type[ithreetotal$shot_made_flag=='shot_yes']))


player_name=c('Stephen curry','Kevin Durant','Draymond Green','Klay Thompson','Andre Iguodala')
sumtotal=c(csumtotal,ksumtotal,gsumtotal,tsumtotal,isumtotal)
summade=c(csumyestotal,ksumyestotal,gsumyestotal,tsumyestotal,isumyestotal)
sum2total=c(csum2total,ksum2total,gsum2total,tsum2total,isum2total)
sum2made=c(csum2yestotal,ksum2yestotal,gsum2yestotal,tsum2yestotal,isum2yestotal)
sum3total=c(csum3total,ksum3total,gsum3total,tsum3total,isum3total)
sum3made=c(csum3yestotal,ksum3yestotal,gsum3yestotal,tsum3yestotal,isum3yestotal)

value_arrange=data.frame(name=player_name,total=sumtotal,made=summade)
arrange(mutate(value_arrange,perc_made=made/total),desc(perc_made))
```

    ##             name total made perc_made
    ## 1   Kevin Durant   915  495 0.5409836
    ## 2  Klay Thompson   915  495 0.5409836
    ## 3 Andre Iguodala   371  192 0.5175202
    ## 4  Stephen curry  1250  584 0.4672000
    ## 5 Draymond Green   578  245 0.4238754

2point eff shooting by %
------------------------

``` r
library(dplyr)
ap<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//shots-data.csv',stringsAsFactors = FALSE)

currys<-ap[ap$player=='Stephen Curry',c('player','shot_made_flag','shot_type')]
csumtotal=sum(table(currys$shot_made_flag))
csumyestotal=sum(table(currys$shot_made_flag[currys$shot_made_flag=='shot_yes']))
ctwototal=currys[currys$shot_type=='2PT Field Goal',]
cthreetotal=currys[currys$shot_type=='3PT Field Goal',]
csum2total=sum(table(ctwototal$shot_type))
csum2yestotal=sum(table(ctwototal$shot_type[ctwototal$shot_made_flag=='shot_yes']))
csum3total=sum(table(cthreetotal$shot_type))
csum3yestotal=sum(table(cthreetotal$shot_type[cthreetotal$shot_made_flag=='shot_yes']))

kevins<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
ksumtotal=sum(table(kevins$shot_made_flag))
ksumyestotal=sum(table(kevins$shot_made_flag[kevins$shot_made_flag=='shot_yes']))
ktwototal=kevins[kevins$shot_type=='2PT Field Goal',]
kthreetotal=kevins[kevins$shot_type=='3PT Field Goal',]
ksum2total=sum(table(ktwototal$shot_type))
ksum2yestotal=sum(table(ktwototal$shot_type[ktwototal$shot_made_flag=='shot_yes']))
ksum3total=sum(table(kthreetotal$shot_type))
ksum3yestotal=sum(table(kthreetotal$shot_type[kthreetotal$shot_made_flag=='shot_yes']))

thompson<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
tsumtotal=sum(table(thompson$shot_made_flag))
tsumyestotal=sum(table(thompson$shot_made_flag[thompson$shot_made_flag=='shot_yes']))
ttwototal=thompson[thompson$shot_type=='2PT Field Goal',]
tthreetotal=thompson[thompson$shot_type=='3PT Field Goal',]
tsum2total=sum(table(ttwototal$shot_type))
tsum2yestotal=sum(table(ttwototal$shot_type[ttwototal$shot_made_flag=='shot_yes']))
tsum3total=sum(table(tthreetotal$shot_type))
tsum3yestotal=sum(table(tthreetotal$shot_type[tthreetotal$shot_made_flag=='shot_yes']))

greens<-ap[ap$player=='Draymond Green',c('player','shot_made_flag','shot_type')]
gsumtotal=sum(table(greens$shot_made_flag))
gsumyestotal=sum(table(greens$shot_made_flag[greens$shot_made_flag=='shot_yes']))
gtwototal=greens[greens$shot_type=='2PT Field Goal',]
gthreetotal=greens[greens$shot_type=='3PT Field Goal',]
gsum2total=sum(table(gtwototal$shot_type))
gsum2yestotal=sum(table(gtwototal$shot_type[gtwototal$shot_made_flag=='shot_yes']))
gsum3total=sum(table(gthreetotal$shot_type))
gsum3yestotal=sum(table(gthreetotal$shot_type[gthreetotal$shot_made_flag=='shot_yes']))


iguodalas<-ap[ap$player=='Andre Iguodala',c('player','shot_made_flag','shot_type')]
isumtotal=sum(table(iguodalas$shot_made_flag))
isumyestotal=sum(table(iguodalas$shot_made_flag[iguodalas$shot_made_flag=='shot_yes']))
itwototal=iguodalas[iguodalas$shot_type=='2PT Field Goal',]
ithreetotal=iguodalas[iguodalas$shot_type=='3PT Field Goal',]
isum2total=sum(table(itwototal$shot_type))
isum2yestotal=sum(table(itwototal$shot_type[itwototal$shot_made_flag=='shot_yes']))
isum3total=sum(table(ithreetotal$shot_type))
isum3yestotal=sum(table(ithreetotal$shot_type[ithreetotal$shot_made_flag=='shot_yes']))


player_name=c('Stephen curry','Kevin Durant','Draymond Green','Klay Thompson','Andre Iguodala')
sumtotal=c(csumtotal,ksumtotal,gsumtotal,tsumtotal,isumtotal)
summade=c(csumyestotal,ksumyestotal,gsumyestotal,tsumyestotal,isumyestotal)
sum2total=c(csum2total,ksum2total,gsum2total,tsum2total,isum2total)
sum2made=c(csum2yestotal,ksum2yestotal,gsum2yestotal,tsum2yestotal,isum2yestotal)
sum3total=c(csum3total,ksum3total,gsum3total,tsum3total,isum3total)
sum3made=c(csum3yestotal,ksum3yestotal,gsum3yestotal,tsum3yestotal,isum3yestotal)

value_2parrange=data.frame(name=player_name,total=sum2total,made=sum2made)
arrange(mutate(value_arrange,perc_made2=made/total),desc(perc_made2))
```

    ##             name total made perc_made2
    ## 1   Kevin Durant   915  495  0.5409836
    ## 2  Klay Thompson   915  495  0.5409836
    ## 3 Andre Iguodala   371  192  0.5175202
    ## 4  Stephen curry  1250  584  0.4672000
    ## 5 Draymond Green   578  245  0.4238754

3point eff shooting by %
------------------------

``` r
library(dplyr)
ap<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//shots-data.csv',stringsAsFactors = FALSE)

currys<-ap[ap$player=='Stephen Curry',c('player','shot_made_flag','shot_type')]
csumtotal=sum(table(currys$shot_made_flag))
csumyestotal=sum(table(currys$shot_made_flag[currys$shot_made_flag=='shot_yes']))
ctwototal=currys[currys$shot_type=='2PT Field Goal',]
cthreetotal=currys[currys$shot_type=='3PT Field Goal',]
csum2total=sum(table(ctwototal$shot_type))
csum2yestotal=sum(table(ctwototal$shot_type[ctwototal$shot_made_flag=='shot_yes']))
csum3total=sum(table(cthreetotal$shot_type))
csum3yestotal=sum(table(cthreetotal$shot_type[cthreetotal$shot_made_flag=='shot_yes']))

kevins<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
ksumtotal=sum(table(kevins$shot_made_flag))
ksumyestotal=sum(table(kevins$shot_made_flag[kevins$shot_made_flag=='shot_yes']))
ktwototal=kevins[kevins$shot_type=='2PT Field Goal',]
kthreetotal=kevins[kevins$shot_type=='3PT Field Goal',]
ksum2total=sum(table(ktwototal$shot_type))
ksum2yestotal=sum(table(ktwototal$shot_type[ktwototal$shot_made_flag=='shot_yes']))
ksum3total=sum(table(kthreetotal$shot_type))
ksum3yestotal=sum(table(kthreetotal$shot_type[kthreetotal$shot_made_flag=='shot_yes']))

thompson<-ap[ap$player=='Kevin Durant',c('player','shot_made_flag','shot_type')]
tsumtotal=sum(table(thompson$shot_made_flag))
tsumyestotal=sum(table(thompson$shot_made_flag[thompson$shot_made_flag=='shot_yes']))
ttwototal=thompson[thompson$shot_type=='2PT Field Goal',]
tthreetotal=thompson[thompson$shot_type=='3PT Field Goal',]
tsum2total=sum(table(ttwototal$shot_type))
tsum2yestotal=sum(table(ttwototal$shot_type[ttwototal$shot_made_flag=='shot_yes']))
tsum3total=sum(table(tthreetotal$shot_type))
tsum3yestotal=sum(table(tthreetotal$shot_type[tthreetotal$shot_made_flag=='shot_yes']))

greens<-ap[ap$player=='Draymond Green',c('player','shot_made_flag','shot_type')]
gsumtotal=sum(table(greens$shot_made_flag))
gsumyestotal=sum(table(greens$shot_made_flag[greens$shot_made_flag=='shot_yes']))
gtwototal=greens[greens$shot_type=='2PT Field Goal',]
gthreetotal=greens[greens$shot_type=='3PT Field Goal',]
gsum2total=sum(table(gtwototal$shot_type))
gsum2yestotal=sum(table(gtwototal$shot_type[gtwototal$shot_made_flag=='shot_yes']))
gsum3total=sum(table(gthreetotal$shot_type))
gsum3yestotal=sum(table(gthreetotal$shot_type[gthreetotal$shot_made_flag=='shot_yes']))


iguodalas<-ap[ap$player=='Andre Iguodala',c('player','shot_made_flag','shot_type')]
isumtotal=sum(table(iguodalas$shot_made_flag))
isumyestotal=sum(table(iguodalas$shot_made_flag[iguodalas$shot_made_flag=='shot_yes']))
itwototal=iguodalas[iguodalas$shot_type=='2PT Field Goal',]
ithreetotal=iguodalas[iguodalas$shot_type=='3PT Field Goal',]
isum2total=sum(table(itwototal$shot_type))
isum2yestotal=sum(table(itwototal$shot_type[itwototal$shot_made_flag=='shot_yes']))
isum3total=sum(table(ithreetotal$shot_type))
isum3yestotal=sum(table(ithreetotal$shot_type[ithreetotal$shot_made_flag=='shot_yes']))


player_name=c('Stephen curry','Kevin Durant','Draymond Green','Klay Thompson','Andre Iguodala')
sumtotal=c(csumtotal,ksumtotal,gsumtotal,tsumtotal,isumtotal)
summade=c(csumyestotal,ksumyestotal,gsumyestotal,tsumyestotal,isumyestotal)
sum2total=c(csum2total,ksum2total,gsum2total,tsum2total,isum2total)
sum2made=c(csum2yestotal,ksum2yestotal,gsum2yestotal,tsum2yestotal,isum2yestotal)
sum3total=c(csum3total,ksum3total,gsum3total,tsum3total,isum3total)
sum3made=c(csum3yestotal,ksum3yestotal,gsum3yestotal,tsum3yestotal,isum3yestotal)

value_3parrange=data.frame(name=player_name,total=sum3total,made=sum3made)
arrange(mutate(value_arrange,perc_made3=made/total),desc(perc_made3))
```

    ##             name total made perc_made3
    ## 1   Kevin Durant   915  495  0.5409836
    ## 2  Klay Thompson   915  495  0.5409836
    ## 3 Andre Iguodala   371  192  0.5175202
    ## 4  Stephen curry  1250  584  0.4672000
    ## 5 Draymond Green   578  245  0.4238754
