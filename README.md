# gghighlighter

Highlighter and multiple sequence alignment (MSA) plots in ggplot2. gghighltighter adds functions to fortify ape::DNAbin and ape::AAbin  objects as well as matrices of aligments.

Originally forked from YuLab-SMU/ggmsa, but almost no code was retained, execpt colour schemes.

## Installation

To install run the following commands in R

```r
#install devtools and Bioconductor
install.packages(c("devtools", "BiocManager"))

#install treeio package
library(BiocManager)
BiocManager::install("treeio")

#install ggmmsa from github
devtools::install_github("brj1/ggmsa")
```

## Examples

You can simply call the `ggmsa` function to plot a MSA.

```r
library(ggmsa)
f <- system.file("extdata/sample.fasta", package="ggmsa")
dev.new(width=11, height=2)
ggmsa(f, 164, 213)
```

Beware that there are a lot of warnings thrown in the current version of ggmsa.

## GGMSA info

fonts

<https://github.com/yixuan/fontr>
