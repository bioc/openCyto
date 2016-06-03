# OpenCyto: How to use different auto gating functions
Mike Jiang  




Here we will illustrate how to choose and use the appropriate gating functions that are natively provided by `openCyto` package. Apparently users can always define develop their own `gating` algorithms and use them as the `plugin` functions in `openCyto` framework, which are beyond the scope of this document.



```r
library(openCyto)
library(ggcyto)
gs <- load_gs("/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/auto/gslist-bcell/cgRoygodqg/")
```
## 1D gating methods
### `mindensity`
The name of this gating function is self-explaining, that is to find the minimum as the cutpoint between negative and postive peaks in 1d density plot. It is fast,robust and extremely easy to use especially when there is a good separation between `+` and `-` populations/peaks.

For example, it is usually easy to gate on `CD3` channel and no need to supply any arguments to the method.


```r
fr <- getData(gs[[1]], "Live")
chnl <- "CD3"
g <- mindensity(fr, channel = chnl)
autoplot(fr, chnl) + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-3-1.png)

However, it may need some guidance when there are more than `2` major peaks/populations detected in densit profile.

```r
fr <- getData(gs[[1]], "boundary")
chnl <- "FSC-A"
g <- mindensity(fr, channel = chnl)
mylimits <- ggcyto_par_set(limits = "instrument")
p <- autoplot(fr, chnl) + mylimits
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-4-1.png)

Here we actually want to cut between the `2nd` and `3rd` peaks to remove the `debris cells` that are represented by those two negative peaks. So we can simply specify a `range` that will limit the locations where the cut point will be placed. 

```r
g <- mindensity(fr, channel = "FSC-A", gate_range=c(5e4,1e5), adjust = 1.5)
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-5-1.png)

And as shown, we also changed the `kernal density`  smoothing factor `adjust` from  `2`(default value set in `openCtyo`) to `1.5` to avoid over-smoothing.

Alternatively you can achieve the same effect by setting `min` or `max` to pre-filter the data before the `mindenstiy` works on it.

```r
g <- mindensity(fr, channel = "FSC-A", min = 2e4)
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-6-1.png)



```r
g <- mindensity(fr, channel = "FSC-A", gate_range=c(5e4,1e5), adjust = 1.5)
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-7-1.png)

To choose one way or the other or combining both is highly dependent on how your data. The more contrains will give you more controls on how gating proceeds yet at cost of robustness of your gating pipeline sometime.

### `tailgate`
This gating method is used in the senarios where there is only one major peak detected thus automatically disqualify the usage of `mindensity`. `tol` is to control how far the cut point should be placed away from the peak. 


```r
fr <- getData(gs[[1]], "lymph")
chnl <- "Live"
g <- tailgate(fr, channel = chnl, tol = 5e-2)
p <- autoplot(fr, chnl) + mylimits
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-8-1.png)

This gating method is more commonly used in gating the `rare` populations when the target population is not prominent enough to stand out as the second peak. (e.g. `cytokine` gates in `ICS` assays.)

## 2D gating methods
### `flowClust.2d`
`flowClust` package in itself is not limited to 2-dimensional gating. But here we are talking about a dedicated wrapper function `.flowClust.2d` from `openCyto` package that leverages `flowClust` clustering engine to work on `2D` cases specifically. You won't need to write the full name of the function in `csv` gating template, simply put `flowClust` in the `gating_method` column, and then the template parser will automatically dispatch to the right function.


```r
fr <- getData(gs[[1]], "nonDebris")
chnl <- c("FSC-A", "SSC-A")
g <- openCyto:::.flowClust.2d(fr, channels = chnl, K=2, target=c(1e5,5e4), quantile=0.95, pp_res = NULL)
p <- autoplot(fr, x = chnl[1], y = chnl[2]) + mylimits
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-9-1.png)

`K` is to tell the algorithm how many major clusters/populations are expected in the 2d profile. `target` specify the mean/center of the target population to get, which doesn't have to be precise. If not supplied, flowClust will pick the most prominent cluster as the target, which would be the right choice in most cases.
`quantile` specify how large the `ellipse` should be. `pp_res` is used to provide the `prior` information for `flowClust`. (More details are in `?flowClust`)

### `Transitional gate`
`flowClust.2d` can optionally construct a `Transitional gate`, which is a speical kind of polygon gate with one edge placed diagonally that is often seen in `flowJo`. Here is an example:

```r
fr <- getData(gs[[1]], "CD19andCD20")
chnl <- c("CD38", "CD24")
g <- openCyto:::.flowClust.2d(fr, channels = chnl, K=6,transitional=TRUE,target=c(3.5e3,3.5e3), quantile=0.95,translation=0.15, pp_res = NULL)
p <- autoplot(fr, x = chnl[1], y = chnl[2]) + mylimits
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-10-1.png)

The rational behind the algorithm is beyond the scope of this document. Please see its detailed explainations in `?flowClust.2d`.

### `quadGate.tmix`
This gating method identifies two quadrants (first, and third quadrants) by fitting the data with tmixture model.
It is particually useful when the two markers are not well resolved thus the regular quadGate method
that is based on `1d` gating will not find the perfect cut points on both dimensions.

```r
gs <- load_gs("/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/auto/gslist-DC/DKM455iBS1/")
fr <- getData(gs[[3]], "HLADR+")
chnl <- c("CD11c", "CD123")
p <- autoplot(fr, chnl[1], chnl[2])
g <- quadGate.tmix(fr, chnl, K = 3, usePrior = "no")
p + geom_gate(g)
```

![](HowToAutoGating_files/figure-html/unnamed-chunk-11-1.png)
