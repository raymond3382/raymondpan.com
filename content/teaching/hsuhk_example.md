---
title: "An Example of Data Analysis by R"
date: "2022-04-07"
output:
  blogdown::html_page:
    toc: true
---

# 1. Environment Preparation

## 1.1 Environment Cleansing


```r
rm(list = ls()) # remove all objects
```

## 1.2 Environment Checking


```r
sessionInfo() # check current environment
```

```
## R version 4.1.3 (2022-03-10)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Monterey 12.3.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] bookdown_0.25   digest_0.6.29   R6_2.5.1        jsonlite_1.8.0 
##  [5] magrittr_2.0.2  evaluate_0.15   blogdown_1.8    stringi_1.7.6  
##  [9] rlang_1.0.2     cli_3.2.0       rstudioapi_0.13 jquerylib_0.1.4
## [13] bslib_0.3.1     rmarkdown_2.13  tools_4.1.3     stringr_1.4.0  
## [17] xfun_0.30       yaml_2.3.5      fastmap_1.1.0   compiler_4.1.3 
## [21] htmltools_0.5.2 knitr_1.38      sass_0.4.1
```

```r
version # check the version of r
```

```
##                _                           
## platform       aarch64-apple-darwin20      
## arch           aarch64                     
## os             darwin20                    
## system         aarch64, darwin20           
## status                                     
## major          4                           
## minor          1.3                         
## year           2022                        
## month          03                          
## day            10                          
## svn rev        81868                       
## language       R                           
## version.string R version 4.1.3 (2022-03-10)
## nickname       One Push-Up
```

## 1.3 Packages Loading


```r
library(data.table)
library(sjPlot)
library(ggpubr)
library(psych)
library(plm)
library(jtools)
library(zoo)
library(stargazer)
library(knitr)
```



# 2. Data Import


```r
dt <- fread("~/Library/CloudStorage/OneDrive-Personal/Focus/e00/diss3_data/diss_e2_hsuhk.csv") # import data
```

# 3 Data Tidy
## 3.1 Variable Generation


```r
dt |> setorder(mid, year) # rank the data by mid and year

# Gen DV: log total revenue and then log it as DV ------------------------------
dt[, totrev_log := log(totrev +1)]

# Gen IV: the average of AS in year t and in year t-1 ---------------------------
dt[, ads2 := ifelse(is.na(ads),adsmkt,ads)]
dt[, ads2_lag :=shift(ads2,1),by="mid"]
dt[, ads2_lag2 :=shift(ads2,2),by="mid"]
dt[, ads2_mean :=rowMeans(dt[,.(ads2,ads2_lag)],na.rm=T)]
dt[, ads2_mean := ifelse(is.nan(ads2_mean),NA, ads2_mean)]
dt[, ads2_mean_log := ifelse(is.na(ads2_mean),NA, log(ads2_mean))]

# Gen Moderators -------------------------------------------------
## Franchisee Proportion
dt[, pof := ifelse(is.na(tend), NA, round(fend/tend,3))]
dt[, pof_log :=log(pof +1)]

## Franchisee Dispersion
dt[, fend_nstate_log := round(log(fend_nstate+1),3)]

### Gen Control variables -------------------------------------------------
dt[, `:=`(age= year-eyear, exp =year-fyear)]  # age 
dt[, `:=`(age= ifelse(age<0,0,age), exp=ifelse(exp<0,0,exp))] # experience
dt[, `:=` (age_log = log(age +1), exp_log =log(exp +1))]

dt[, tend_log := round(log(tend +1),3)] # system size

dt[, train := (train1+train2)/2] # system support: training
dt[, train_log := ifelse(train==0, 0, round(log(train+1),3))]

dt[, rratio := (rratio1+rratio2)/2] # royalty ratio

dt[, pgdp := ifelse(is.na(pop),NA, round(gdp/pop,3))] # gpd per capita
dt[, pgdp_log := ifelse(pgdp==0, 0, round(log(pgdp+1),3))]

dt[, pop_log := ifelse(pop==0, 0, round(log(pop+1),3))] # population 
```

## 3.2 Sample Selection


```r
# rank the observations by industry
dt[,.N/10, by=naics_2]  |>  
  setnames(1:2, c("Industry", "N")) |> 
  setorder(-N,Industry) |> 
  tab_df()
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">Industry</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">N</th>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">72</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">28</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">54</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">20</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">12</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">81</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">12</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">56</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">9</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">44</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">8</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">3</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">53</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">71</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">1</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; ">52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">1</td>
</tr>
</table>

```r
# delete the industry of which the number of franchisors is less than 3
dt<- dt[ !(naics_2 %in% c(52,49,71,62,53))]

# calculate the number of franchisors that do not disclose ads at all. 
dt[, select1 := ifelse(is.na(ads2),0,1)]
dt[, select1_sum := sum(select1), by=c("mid")]

# calculate the number of franchisors that do not use royalty ratio. 
dt[, select2 := ifelse(is.na(rratio),0,1)]
dt[, select2_sum := sum(select2), by=c("mid")]

# mark the franchisors we choose in our sample
dt[, select := ifelse(select1_sum > 0 & select2_sum >0, 1,0)]
dt <- dt[select ==1]
```

## 3.3 Missing Value Imputation


```r
dt |> setorder(mid, year)
# the first non missing value replace; locf--> location forward
dt[,train_log := na.locf(train_log,na.rm=F), by="mid"] 
# the last non missing value replace
dt[,train_log := na.locf(train_log,na.rm=F,fromLast =T), by="mid"] 

# the first non missing value replace
dt[,rratio := na.locf(rratio,na.rm=F), by="mid"] 
# the last non missing value replace
dt[,rratio := na.locf(rratio,na.rm=F,fromLast =T), by="mid"] 
```

## 3.4 Variable Selection


```r
dt[, `:=`(SP = totrev_log,
          AS = ads2_mean_log,
          FP = pof,
          FD = fend_nstate_log,
          SIZE = tend_log,
          RR = rratio,
          SUPPORT = train_log,
          AGE = age_log,
          POP = pop_log,
          PGDP = pgdp_log)]
dv <- c("SP")
iv <- c("AS", "FP", "FD")
cv <- c("SIZE", "RR", "SUPPORT", 
        "AGE", "POP", 
        "PGDP")
fdt <- dt[, .SD, .SDcols = c("mid","year","naics_2", "franchisor", dv,iv,cv)]
```

# 4. Exploratory Data Analysis

## 4.1 Sample Description



```r
# display mid, franchisor name, and year 
fdt[,.(naics_2, mid, franchisor)] |> 
  unique() |> as.data.table(keep.rownames = T) |> 
  setnames(1:3, c("Industry","MatchID","Franchisor")) |> 
  setorder(Industry, MatchID) |> 
  kable()
```



| Industry| MatchID|Franchisor                             |
|--------:|-------:|:--------------------------------------|
|       44|       1|1-800-radiator                         |
|       44|       6|7-eleven                               |
|       44|      40|big-o-tires                            |
|       44|     196|mrs-fields-franchising                 |
|       44|     241|radio-shack                            |
|       45|     120|fleet-feet                             |
|       45|     161|kid-to-kid                             |
|       45|     166|learning-express                       |
|       54|      15|alliance-cost-containment              |
|       54|      17|american-leak-detecktion               |
|       54|      85|dÈcor-you                              |
|       54|     251|brickicker                             |
|       54|     268|sir-speedy                             |
|       54|     291|The-alternative-board                  |
|       54|     295|teamlogit                              |
|       54|     330|world-inspection-network-international |
|       56|      70|city-wide                              |
|       56|      93|dryer-vent-wizard                      |
|       56|     117|fish-window-cleaning                   |
|       56|     231|post-net                               |
|       56|     281|square-two-financial                   |
|       61|      73|club-z                                 |
|       61|     111|fast-teks                              |
|       61|     130|golf-tec                               |
|       61|     147|i9-sports                              |
|       61|     158|jump-bunch                             |
|       61|     167|learning-rx                            |
|       61|     179|mathnasium                             |
|       61|     201|new-horizons-franchising-group         |
|       61|     303|the-little-gym-international           |
|       61|     313|tutoring-club                          |
|       72|      23|Arby's                                 |
|       72|      29|antie-anne                             |
|       72|      61|carvel                                 |
|       72|      69|cinnabon                               |
|       72|      90|dippin-dots                            |
|       72|     108|Famous Famiglia                        |
|       72|     109|Famous Daves                           |
|       72|     146|Hurrican Grills and Wings              |
|       72|     182|McALister Deli                         |
|       72|     183|McDonald                               |
|       72|     191|moe-s-franchisor                       |
|       72|     217|Pancheros                              |
|       72|     224|Pita Pit                               |
|       72|     239|Qdoba                                  |
|       72|     246|Red Robin                              |
|       72|     250|rita-s-water-ice-franchise             |
|       72|     253|Salsarita                              |
|       72|     257|Schlotzsky                             |
|       72|     258|Schmizza                               |
|       72|     294|tcby-system                            |
|       72|     312|Tropical Smoothie                      |
|       72|     336|Zaxby                                  |
|       81|     124|furniture-medic                        |
|       81|     189|mister-sparky-franchising              |
|       81|     194|monkey-joes                            |
|       81|     203|lapels                                 |
|       81|     204|N-hance                                |
|       81|     209|one-hour-air-conditioning-franchising  |
|       81|     225|piu-holdings                           |
|       81|     228|planet-beach-franchising               |
|       81|     286|supercut                               |

```r
# count the number of franchisors in each industry
fdt[, .N/10, by = "naics_2"] |> 
  setnames(1:2, c("Industry","N")) |> 
  setorder(-N, Industry) |> 
  tab_df()
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">Industry</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">N</th>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">72</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">22</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">10</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">81</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">9</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">54</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">8</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">44</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">56</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; ">45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">3</td>
</tr>
</table>

```r
# describe the DV, IV, Moderators, and CV
fdt[, -c("mid", "year", "naics_2", "franchisor")] |> 
  describe() |> as.data.table(keep.rownames = T) |> tab_df()
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">rn</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">vars</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">n</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">mean</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">sd</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">median</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; col7">trimmed</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; col8">mad</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; col9">min</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; 0">max</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; 1">range</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; 2">skew</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; 3">kurtosis</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; 4">se</th>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">SP</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">1</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">534</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">16.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2.79</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">15.70</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">16.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">2.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">9.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">23.55</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">14.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">0.70</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">-0.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.12</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">AS</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">521</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">12.90</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2.67</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">12.43</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">12.71</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">2.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">6.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">19.40</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">12.92</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">0.57</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.12</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">FP</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">3</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">541</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.89</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.99</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">0.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">1.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">1.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">-2.38</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">5.63</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.01</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">FD</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">4</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">541</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">3.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.75</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">3.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">3.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">3.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">3.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">-1.79</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">3.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.03</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">SIZE</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">541</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">1.53</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">5.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">1.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.69</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">9.56</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">8.86</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">0.44</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">0.90</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.07</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">RR</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">6</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">620</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">0.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">4.83</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">24.76</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.00</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">SUPPORT</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">7</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">610</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">4.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">4.58</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">4.54</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.96</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">6.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">6.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">-1.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">3.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.04</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">AGE</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">8</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">620</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">2.83</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">2.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.79</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">4.52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">4.52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">-0.77</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">1.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.03</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">POP</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">9</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">541</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">0.71</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">5.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">5.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">0.30</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col9">2.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 0">5.75</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 1">3.75</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 2">-2.38</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 3">5.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; 4">0.03</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; ">PGDP</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">541</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">10.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">10.77</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; col7">10.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; col8">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; col9">10.44</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; 0">10.99</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; 1">0.55</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; 2">-0.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; 3">0.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; 4">0.00</td>
</tr>
</table>



## 4.2 Correlation Analysis


```r
## the relationship between IV and DV
fdt |> ggscatterhist(x= "AS", y="SP")
```

<img src="/teaching/hsuhk_example_files/figure-html/unnamed-chunk-7-1.png" width="672" style="display: block; margin: auto;" />

```r
## the correlation figure
fdt[, -c("mid", "year", "naics_2","franchisor")] |> sjp.corr()
```

<img src="/teaching/hsuhk_example_files/figure-html/unnamed-chunk-7-2.png" width="672" style="display: block; margin: auto;" />

```r
## the correlation table
fdt[, -c("mid", "year", "naics_2","franchisor")] |> 
  tab_corr(triangle = "lower", 
           p.numeric = F, digits = 2)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">&nbsp;</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">SP</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">AS</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">FP</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">FD</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">SIZE</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">RR</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">SUPPORT</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">AGE</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">POP</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">PGDP</th>
</tr>
<tr>
<td style="font-style:italic;">SP</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">AS</td>
<td style="padding:0.2cm; text-align:center;">0.81<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">FP</td>
<td style="padding:0.2cm; text-align:center;">-0.48<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">-0.44<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">FD</td>
<td style="padding:0.2cm; text-align:center;">0.45<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.35<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.03<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">SIZE</td>
<td style="padding:0.2cm; text-align:center;">0.79<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.64<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">-0.33<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.68<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">RR</td>
<td style="padding:0.2cm; text-align:center;">0.21<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.09<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">-0.04<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.08<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">0.26<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">SUPPORT</td>
<td style="padding:0.2cm; text-align:center;">0.20<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.06<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">-0.02<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">-0.13<span style="vertical-align:super;font-size:0.8em;">**</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.02<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">0.13<span style="vertical-align:super;font-size:0.8em;">**</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">AGE</td>
<td style="padding:0.2cm; text-align:center;">0.57<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.38<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">-0.27<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.34<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.58<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.21<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.23<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">POP</td>
<td style="padding:0.2cm; text-align:center;">0.37<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.26<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.02<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">0.94<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.55<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.10<span style="vertical-align:super;font-size:0.8em;">*</span></td>
<td style="padding:0.2cm; text-align:center;">-0.16<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.29<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td style="font-style:italic;">PGDP</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.08<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.08<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">0.09<span style="vertical-align:super;font-size:0.8em;">*</span></td>
<td style="padding:0.2cm; text-align:center;">0.24<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.08<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">0.05<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center; color:#999999;">-0.04<span style="vertical-align:super;font-size:0.8em;"></span></td>
<td style="padding:0.2cm; text-align:center;">0.21<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">0.28<span style="vertical-align:super;font-size:0.8em;">***</span></td>
<td style="padding:0.2cm; text-align:center;">&nbsp;</td>
</tr>
<tr>
<td colspan="11" style="border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;">Computed correlation used pearson-method with listwise-deletion.</td>
</tr>
 
</table>


# 5. Hypotheses Testing

## 5.1 Regression Codes


```r
fdt |> is.pbalanced() # TRUE
```

```
[1] TRUE
```

```r
fdt |> setorder(mid, year)

m11<-formula(SP ~  #AS  
            # + FP + FD
            # + I(scale(fdt$AS) * scale(fdt$FP)) 
            # + I(scale(fdt$AS) * scale(fdt$FD))
             I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + AGE
             + SUPPORT
             + RR
             + POP  
             + PGDP
             + factor(naics_2) + factor(year) 
             | factor(naics_2) 
             + factor(year)
             + POP 
             + PGDP
             + AGE
             |  #AS + FP + FD 
               I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + SUPPORT + RR 
             #+ I(scale(fdt$AS) * scale(fdt$FP)) 
             #+ I(scale(fdt$AS)* scale(fdt$FD))
)


ht11<-plm(m11, fdt, index=c("mid","year"),random.method="ht", model="random", inst.method = "baltagi")

# fwrite(data.table(round(summary(ht11,digit=3)$coefficients,3),keep.rownames = T),"ht11_coefficient.csv")

m12<-formula(SP ~  AS  
             #+ FP + FD
            # + I(scale(fdt$AS) * scale(fdt$FP)) 
            # + I(scale(fdt$AS) * scale(fdt$FD))
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + AGE
             + SUPPORT
             + RR
             + POP  
             + PGDP
             + factor(naics_2) + factor(year) 
             | factor(naics_2) 
             + factor(year)
             + POP 
             + PGDP
             + AGE
             |  AS #+ FP + FD 
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + SUPPORT + RR 
            # + I(scale(fdt$AS) * scale(fdt$FP)) 
            # + I(scale(fdt$AS)* scale(fdt$FD))
)


ht12<-plm(m12, fdt, index=c("mid","year"),random.method="ht", model="random", inst.method = "baltagi")

m13<-formula(SP ~  AS  
             + FP #+ FD
             + I(scale(fdt$AS) * scale(fdt$FP)) 
            # + I(scale(fdt$AS) * scale(fdt$FD))
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + AGE
             + SUPPORT
             + RR
             + POP  
             + PGDP
             + factor(naics_2) + factor(year) 
             | factor(naics_2) 
             + factor(year)
             + POP 
             + PGDP
             + AGE
             |  AS + FP #+ FD 
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + SUPPORT + RR 
             + I(scale(fdt$AS) * scale(fdt$FP)) 
            # + I(scale(fdt$AS)* scale(fdt$FD))
)


ht13<-plm(m13, fdt, index=c("mid","year"),random.method="ht", model="random", inst.method = "baltagi")

m14<-formula(SP ~  AS  
             #+ FP 
             + FD
             #+ I(scale(fdt$AS) * scale(fdt$FP)) 
             + I(scale(fdt$AS) * scale(fdt$FD))
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + AGE
             + SUPPORT
             + RR
             + POP  
             + PGDP
             + factor(naics_2) + factor(year) 
             | factor(naics_2) 
             + factor(year)
             + POP 
             + PGDP
             + AGE
             |  AS + FD #+ FP 
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + SUPPORT + RR 
             # + I(scale(fdt$AS) * scale(fdt$FP)) 
             + I(scale(fdt$AS)* scale(fdt$FD))
)

ht14<-plm(m14, fdt, index=c("mid","year"),random.method="ht", model="random", inst.method = "baltagi")

m15<-formula(SP ~  AS  
             + FP + FD
             + I(scale(fdt$AS) * scale(fdt$FP)) 
             + I(scale(fdt$AS) * scale(fdt$FD))
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + AGE
             + SUPPORT
             + RR
             + POP  
             + PGDP
             + factor(naics_2) + factor(year) 
             | factor(naics_2) 
             + factor(year)
             + POP 
             + PGDP
             + AGE
             |  AS + FP + FD 
             + I(scale(fdt$SIZE, center = T, scale = F)) 
             + I(scale(fdt$SIZE, center = T, scale = F)^2)
             + SUPPORT + RR 
             + I(scale(fdt$AS) * scale(fdt$FP)) 
             + I(scale(fdt$AS)* scale(fdt$FD))
)


ht15<-plm(m15, fdt, index=c("mid","year"),random.method="ht", model="random", inst.method = "baltagi")
```

## 5.2 Regression Table

```r
# regression table 
stargazer(ht11,ht12,ht13,ht14,ht15, type="html", 
          single.row= F, intercept.bottom = F,
          dep.var.labels = "Sales Performance",
          keep = c("Constant",
                    "AS",
                    "FP",
                    "FD",
                    "I(scale(fdt$AS)*scale(fdt$FP))",
                  "I(scale(fdt$AS)*scale(fdt$FD))",
                  "I(scale(fdt$SIZE, center = T, scale = F))",
                  "I(scale(fdt$SIZE, center = T, scale = F)^2)",
                  "AGE",
                  "SUPPORT",
                  "RR",
                  "POP", 
                  "PGDP"),
          order = c("Constant",
                    "AS",
                    "FP",
                    "FD",
                    "I(scale(fdt$AS)*scale(fdt$FP))",
                  "I(scale(fdt$AS)*scale(fdt$FD))",
                  "I(scale(fdt$SIZE, center = T, scale = F))",
                  "I(scale(fdt$SIZE, center = T, scale = F)^2)",
                  "AGE",
                  "SUPPORT",
                  "RR",
                  "POP", 
                  "PGDP"),
          covariate.labels = c("Intercept","Ads Spending (AS)",
                               "Franchisee Proportion (FP)",
                               "Franchisee Dispersion (FD)",
                               "AS*FP",
                               "AS*FD",
                               "System Size",
                               "System Size Square",
                               "System Age",
                               "Support",
                               "Royalty Ratio",
                               "Population" ,
                               "GDP Per Capita")) 
```


<table style="text-align:center"><tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="5"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="5" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="5">Sales Performance</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Intercept</td><td>34.998<sup>**</sup></td><td>23.705<sup>*</sup></td><td>23.746<sup>*</sup></td><td>36.933<sup>**</sup></td><td>37.767<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(16.185)</td><td>(14.389)</td><td>(14.370)</td><td>(14.993)</td><td>(14.976)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Ads Spending (AS)</td><td></td><td>0.046<sup>**</sup></td><td>0.051<sup>***</sup></td><td>0.077<sup>***</sup></td><td>0.087<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.019)</td><td>(0.019)</td><td>(0.020)</td><td>(0.020)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Franchisee Proportion (FP)</td><td></td><td></td><td>0.070<sup>*</sup></td><td></td><td>0.102<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.042)</td><td></td><td>(0.041)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Franchisee Dispersion (FD)</td><td></td><td></td><td></td><td>-0.242<sup>***</sup></td><td>-0.264<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.052)</td><td>(0.053)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">AS*FP</td><td></td><td></td><td>-0.032</td><td></td><td>-0.136</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td>(0.353)</td><td></td><td>(0.360)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">AS*FD</td><td></td><td></td><td></td><td>-0.494<sup>**</sup></td><td>-0.519<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.241)</td><td>(0.248)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">System Size</td><td>0.535<sup>***</sup></td><td>0.516<sup>***</sup></td><td>0.522<sup>***</sup></td><td>0.668<sup>***</sup></td><td>0.695<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.171)</td><td>(0.172)</td><td>(0.173)</td><td>(0.170)</td><td>(0.171)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">System Size Square</td><td>-0.014</td><td>-0.067</td><td>-0.070</td><td>-0.068</td><td>-0.075</td></tr>
<tr><td style="text-align:left"></td><td>(0.066)</td><td>(0.053)</td><td>(0.053)</td><td>(0.052)</td><td>(0.052)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">System Age</td><td>-0.877</td><td>-2.523</td><td>-2.347</td><td>-2.039</td><td>-1.810</td></tr>
<tr><td style="text-align:left"></td><td>(4.766)</td><td>(4.042)</td><td>(4.052)</td><td>(3.950)</td><td>(3.954)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Support</td><td>0.058</td><td>0.050</td><td>0.073</td><td>0.341<sup>*</sup></td><td>0.393<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.147)</td><td>(0.127)</td><td>(0.129)</td><td>(0.203)</td><td>(0.205)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Royalty Ratio</td><td>-1.629</td><td>-0.565</td><td>-0.573</td><td>-1.924</td><td>-2.013</td></tr>
<tr><td style="text-align:left"></td><td>(1.545)</td><td>(1.378)</td><td>(1.377)</td><td>(1.443)</td><td>(1.443)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>493</td><td>446</td><td>446</td><td>446</td><td>446</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.521</td><td>0.614</td><td>0.616</td><td>0.636</td><td>0.640</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.499</td><td>0.593</td><td>0.593</td><td>0.614</td><td>0.617</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>508.431<sup>***</sup></td><td>667.525<sup>***</sup></td><td>670.166<sup>***</sup></td><td>727.827<sup>***</sup></td><td>739.409<sup>***</sup></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="5" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## 5.3 Interaction Plot

```r
fd_mean <- 3.157
fd_sd <- 0.746
fd_min <- fd_mean - fd_sd
fd_max <- fd_mean + fd_sd
as <- c(10.229, 15.567)
fd <- as.data.table(as)
fd[, fd_low := 37.021 - 0.509*fd_min + (0.078 -0.236 *fd_min)*as]
fd[, fd :=  37.021 - 0.509*fd_mean + (0.078 -0.236 *fd_mean)*as]
fd[, fd_high:=  37.021 - 0.509*fd_max + (0.078 -0.236 *fd_max)*as]
fd_melt <- melt(fd, id.vars = "as", measure.vars = c("fd_low","fd","fd_high"), variable.name = "group", value.name = "FD")

fd_melt |> 
  ggplot(aes(x=as, y= FD, color= group)) + 
  geom_line() +
  labs(x= "Ads Spending", y="Sales Performance") +
  scale_color_discrete(name = "Franchisee Dispersion", 
                       labels = c("Low (Mean-1SD)",
                                  "Medium (Mean)",
                                  "High (Mean + 1SD)")) + 
     theme_bw(base_family = "Times New Roman", base_size = 11) + 
     theme(# legend.title = element_blank(),
           legend.key.size = unit(0.5,"cm"),
           legend.position = "top",
           legend.justification = "left",
           axis.title = element_text(size = 12,colour = "black"),
           panel.grid = element_blank()) 
```

<img src="/teaching/hsuhk_example_files/figure-html/unnamed-chunk-10-1.png" width="672" style="display: block; margin: auto;" />

