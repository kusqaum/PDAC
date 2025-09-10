---
author:
- K
authors:
- K
editor: visual
title: Model Estimated Controls for Survival Outcomes
toc-title: Table of contents
---

## Introduction

Model estimated controls (MECs) are statistical models that can be used
to assess the efficacy of new therapies without needing to use clinical
trials.

MECs can act as counterfactual evidence (a way to predict 'what if?'-
i.e., how would patients respond if they had received a different
treatment?)

For survival outcomes, MECs can be used as a control to compare the
effect of a control treatment against the effect of an observed
experimental treatment on overall survival in patients.

We will implement MECs using the 'psc' package which specifically allows
for the comparison of patient groups treated with an experimental
treatment against a counterfactual model.

Let's first install and load the 'psc' and other necessary packages:

::::::: cell
``` {.r .cell-code}
# install.package("psc")
library(psc)
```

::: {.cell-output .cell-output-stderr}
    Warning: package 'psc' was built under R version 4.5.1
:::

``` {.r .cell-code}
library(survival)
library(ggpubr)
```

::: {.cell-output .cell-output-stderr}
    Warning: package 'ggpubr' was built under R version 4.5.1
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: ggplot2
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'ggplot2' was built under R version 4.5.1
:::
:::::::

## An example

In this example, we will look at survival outcomes in patients with
pancreatic cancer from the ESPAC (European Study for Pancreatic
Cancer)-4 trial and compare how the survival of patients differs with
two different treatments...

We are going to load the model that is going to act as the control
treatment (the counterfactual evidence discussed above). This model is a
flexible parametric model that was created using the 'flexsurv' package.

We can look at the model in more detail

::::: cell
``` {.r .cell-code}
load("M:/Documents/Projects/Fellowship/modelDevelop/PDAC/Output/Models/flsm.R")

flsm$call
```

::: {.cell-output .cell-output-stdout}
    flexsurvspline(formula = Surv(time, cen) ~ LymphN + ResecM + 
        Diff_Status + PostOpCA199, data = espac3, k = 5)
:::

``` {.r .cell-code}
class(flsm)
```

::: {.cell-output .cell-output-stdout}
    [1] "flexsurvreg"
:::
:::::

So now that we have our control treatment, monotherapy gemcitabine
(GEM), we will load the ESPAC-4 dataset. ESPAC-4 consists of patients
that have been treated with the experimental treatment, adjuvant
gemcitabine and capecitabine, GEMCAP.

Our aim is to compare the ESPAC-4 dataset against the GEM model to
determine which of the treatments is more effective.

It is important that the variables present in the model are also present
in the comparison dataset. It is also important that the time variable
in the model's survival object and in the comparison dataset are encoded
as 'time'.

:::::: cell
``` {.r .cell-code}
load("M:/Documents/Projects/Fellowship/modelDevelop/PDAC/Data/espac4gemcap.R")
head(espac4_gemcap[1:3,])
```

::: {.cell-output .cell-output-stdout}
      ResecM LymphN Diff_Status  treat      time cen PostOpCA199
    2      0      1           2 GEMCAP  8.707019   1    1.383791
    5      0      0           0 GEMCAP 49.277267   1    0.000000
    8      0      1           1 GEMCAP  6.735929   1    1.824549
:::

``` {.r .cell-code}
head(flsm$data$m[1:3,])
```

::: {.cell-output .cell-output-stdout}
       Surv(time, cen) LymphN ResecM Diff_Status PostOpCA199 (weights)
    18        11.30092      1      0           0    6.222576         1
    19        14.81603      1      0           1    4.394449         1
    28        17.18134      1      0           2    9.314430         1
:::

``` {.r .cell-code}
names(flsm$data$m[,2:5])%in%names(espac4_gemcap)
```

::: {.cell-output .cell-output-stdout}
    [1] TRUE TRUE TRUE TRUE
:::
::::::

We should also check the the variables are the correct class...

Categorical variables should be class factor and continuous variables
should be numeric.

:::: cell
``` {.r .cell-code}
str(espac4_gemcap)
```

::: {.cell-output .cell-output-stdout}
    'data.frame':   362 obs. of  7 variables:
     $ ResecM     : Factor w/ 2 levels "0","1": 1 1 1 2 2 2 2 1 2 2 ...
     $ LymphN     : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 1 1 ...
     $ Diff_Status: Factor w/ 3 levels "0","1","2": 3 1 2 2 1 1 1 2 3 2 ...
     $ treat      : chr  "GEMCAP" "GEMCAP" "GEMCAP" "GEMCAP" ...
     $ time       : num  8.71 49.28 6.74 21.91 23.55 ...
     $ cen        : int  1 1 1 1 1 0 0 1 0 0 ...
     $ PostOpCA199: num  1.38 0 1.82 1.13 5.86 ...
:::
::::

## Fit counterfactual model :)

Now that we have checked the correct variables are in our dataset and
that they are encoded correctly, we can turn the flexible parametric
model into a counterfactual model. We will use the pscfit() function:

::: cell
``` {.r .cell-code}
cfm <- pscCFM(flsm)
```
:::

It is possible to visualise and summarise the data that was used to fit
this model with the built in functions within pscCFM():

::::::::: cell
``` {.r .cell-code}
cfm$datasumm$summ_Table
```

::: cell-output-display
<div id="vxbroaukmk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vxbroaukmk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vxbroaukmk thead, #vxbroaukmk tbody, #vxbroaukmk tfoot, #vxbroaukmk tr, #vxbroaukmk td, #vxbroaukmk th {
  border-style: none;
}

#vxbroaukmk p {
  margin: 0;
  padding: 0;
}

#vxbroaukmk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vxbroaukmk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vxbroaukmk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vxbroaukmk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vxbroaukmk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxbroaukmk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxbroaukmk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxbroaukmk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vxbroaukmk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vxbroaukmk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxbroaukmk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxbroaukmk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vxbroaukmk .gt_spanner_row {
  border-bottom-style: hidden;
}

#vxbroaukmk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vxbroaukmk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vxbroaukmk .gt_from_md > :first-child {
  margin-top: 0;
}

#vxbroaukmk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxbroaukmk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vxbroaukmk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vxbroaukmk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vxbroaukmk .gt_row_group_first td {
  border-top-width: 2px;
}

#vxbroaukmk .gt_row_group_first th {
  border-top-width: 2px;
}

#vxbroaukmk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxbroaukmk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vxbroaukmk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vxbroaukmk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxbroaukmk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxbroaukmk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxbroaukmk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vxbroaukmk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxbroaukmk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxbroaukmk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxbroaukmk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxbroaukmk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxbroaukmk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxbroaukmk .gt_left {
  text-align: left;
}

#vxbroaukmk .gt_center {
  text-align: center;
}

#vxbroaukmk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxbroaukmk .gt_font_normal {
  font-weight: normal;
}

#vxbroaukmk .gt_font_bold {
  font-weight: bold;
}

#vxbroaukmk .gt_font_italic {
  font-style: italic;
}

#vxbroaukmk .gt_super {
  font-size: 65%;
}

#vxbroaukmk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vxbroaukmk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vxbroaukmk .gt_indent_1 {
  text-indent: 5px;
}

#vxbroaukmk .gt_indent_2 {
  text-indent: 10px;
}

#vxbroaukmk .gt_indent_3 {
  text-indent: 15px;
}

#vxbroaukmk .gt_indent_4 {
  text-indent: 20px;
}

#vxbroaukmk .gt_indent_5 {
  text-indent: 25px;
}

#vxbroaukmk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#vxbroaukmk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------
  `<strong>`{=html}Characteristic`</strong>`{=html}                                 `<strong>`{=html}N = 339`</strong>`{=html}[^1^]{.gt_footnote_marks
                                                                                    style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"}
  --------------------------------------------------------------------------------- ---------------------------------------------------------------------------------
  LymphN                                                                            \

      0                                                                             97 (29%)

      1                                                                             242 (71%)

  ResecM                                                                            \

      0                                                                             206 (61%)

      1                                                                             133 (39%)

  Diff_Status                                                                       \

      0                                                                             85 (25%)

      1                                                                             214 (63%)

      2                                                                             40 (12%)

  PostOpCA199                                                                       3.04 (2.30, 4.14)

  [^1^]{.gt_footnote_marks                                                          
  style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"} n 
  (%); Median (Q1, Q3)                                                              
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>
:::

``` {.r .cell-code}
ggarrange(plotlist = cfm$datavis, ncol = 2)
```

::: {.cell-output .cell-output-stdout}
    $`1`
:::

::: cell-output-display
![](howto_surv_k_files/figure-markdown/unnamed-chunk-6-1.png)
:::

::: {.cell-output .cell-output-stdout}

    $`2`
:::

::: cell-output-display
![](howto_surv_k_files/figure-markdown/unnamed-chunk-6-2.png)
:::

::: {.cell-output .cell-output-stdout}

    attr(,"class")
    [1] "list"      "ggarrange"
:::
:::::::::

## Make comparison!

We have now created a counterfactual model which can be compared against
the ESPAC-4 data cohort. The comparison can be carried out using the
pscfcit() function.

Don't forget that the pscfit() function requires the 'time' variables in
the data cohort and in the counterfactual model to be encoded as 'time'
and the event variable to be encoded as 'cen'. The variable names need
to be the exact same.

::: cell
``` {.r .cell-code}
psc <- pscfit(cfm, espac4_gemcap)
```
:::

The output of the comparison can be visualised. The plot below
visualises the effect of each treatment on ESPAC-4 patients. The pink
line represents the model's predicted survival estimates (if the ESPAC-4
patients had been treated with GEM) and the orange line represents the
data cohort's observed survival estimates.

:::: cell
``` {.r .cell-code}
plot(psc)
```

::: cell-output-display
![](howto_surv_k_files/figure-markdown/unnamed-chunk-8-1.png)
:::
::::

The summary of the model can be obtained using the generic summary()
function to show the posterior density. The posterior estimates of the
deviance information criterion (DIC) and the efficacy parameter,
$\beta$, are calculated and shown.

$\beta$ is a measurement of the distance between the observed data and
the model estimate. The 2.5 and 97.5% quantiles are also given. The odds
ratio comparing the experimental treatment to the control can be
calculated as exp($\beta$), in this case exp(-0.466452).

:::: cell
``` {.r .cell-code}
summary(psc)
```

::: {.cell-output .cell-output-stdout}
    Summary: 
     
    362 observations selected from the data cohort for comparison 
    CFM of type flexsurvreg identified  
    linear predictor succesfully obtained with median: 
     trt: 0.812
    Average expected response: 
     trt: 28.337
    Average observed response: 31.67 

    Counterfactual Model (CFM): 
    A model of class 'flexsurvreg' 
     Fit with 5 internal knots

    Formula: 
    Surv(time, cen) ~ LymphN + ResecM + Diff_Status + PostOpCA199
    <environment: 0x000002999d157a18>

    Call:
     CFM model + beta

    Coefficients:
          median     2.5%       97.5%      Pr(x<0)    Pr(x>0)  
    beta    -0.4658    -0.5981    -0.3358     1.0000     0.0000
    DIC   1321.8993  1297.4498  1357.3071         NA         NA
:::
::::

We can extract the posterior distribution of the 'psc' and can view its
autocorrelation and trace

:::::: cell
``` {.r .cell-code}
posterior <- psc$posterior
head(posterior[1:3,])
```

::: {.cell-output .cell-output-stdout}
         gamma0   gamma1   gamma2    gamma3   gamma4     gamma5    gamma6   LymphN1
    1 -11.38080 3.835982 1.291162 -1.317656 1.119018 -0.8876277 0.3372484 0.4876152
    2 -12.50808 4.428885 2.757763 -4.312198 4.400557 -3.0866817 0.9663404 0.3806901
    3 -13.27999 4.627974 4.260717 -7.525031 6.482425 -3.3193637 0.9778527 0.5061440
        ResecM1 Diff_Status1 Diff_Status2 PostOpCA199       beta      DIC
    1 0.1805322   -0.4160534   -0.5897823   0.2671471 -0.4552224       NA
    2 0.2175471   -0.6136808   -0.6165233   0.2509519 -0.4552224 1328.886
    3 0.1686700   -0.3398292   -0.7596465   0.2872179 -0.4550471 1313.394
:::

``` {.r .cell-code}
# autocorrelation
acf(posterior$beta)
```

::: cell-output-display
![](howto_surv_k_files/figure-markdown/unnamed-chunk-10-1.png)
:::

``` {.r .cell-code}
# trace
plot(posterior$beta, type = 's')
```

::: cell-output-display
![](howto_surv_k_files/figure-markdown/unnamed-chunk-10-2.png)
:::
::::::

## Take homes

-   Check your data is formatted correctly (variable names should
    exactly match that of the model's)

-   
