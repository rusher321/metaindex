# metaindex
Integrated microbial index
## Installation

This package can be installed using [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
devtools::install_github('rusher321/metaindex@master')
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(ReportScore)

## basic example code
pr <- read.table("testdata/all.KEGG.abun.txt",row.names = 1,header = T,sep = "\t")
grp <- read.table("testdata/sampleinfo.txt",row.names = 1,header = T,sep = "\t")

grp_sub <- grp[which(grp$TimePoint == "Day2"), 1, drop=F]
grp_sub$Protein <- ifelse(grp_sub$Protein == "C-Pork", "B-Beef", grp_sub$Protein)
pr_sub <- pr[, rownames(grp_sub)]

res_D2 <- ReporterScore(pr_sub, grp_sub, paired = F, database = "./database", occ = 0.1)

fig <- ReportVis(res_D2, color = c("#2470a0", "#DE3C3C"), exclude = T)

## if you want update the datebase 
download_data(db_dir = "database/")

## if you want plot the output
fig <- ReportVis(res, color = c("#2470a0", "#DE3C3C"), exclude = T)

```
## Figure 


ToDo 
-----

- [√]
- [x] 


Meta
----

-   Please [report any issues or
    bugs](https://github.com/rusher321/metaindex/issues).
-   License: MIT
-   Get citation information for `metaindex` in R doing
    `citation(package = 'metaindex')`
-   Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.

