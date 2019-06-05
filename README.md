# My fork of YuLab-SMU/ggmsa

Multiple sequence alignment (MSA) plots in ggplot2. ggmsa adds functions to fortify ape::DNAbin and ape::AAbin  objects as well as matrices of aligments.

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
ggmsa(f)
```

Amino acid values can be added with a geom_text layer.
```r
library(ggplot2)
ggmsa(f) + geom_text(aes(x=x, y=y, label=label)
```
