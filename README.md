# gghighlighter

Highlighter and multiple sequence alignment (MSA) plots in ggplot2. gghighltighter adds functions to fortify ape::DNAbin and ape::AAbin  objects as well as matrices of aligments.

Originally forked from YuLab-SMU/ggmsa.

## Installation

To install run the following commands in R

```r
#install devtools and Bioconductor
install.packages(c("devtools", "BiocManager"))

#install treeio package
library(BiocManager)
BiocManager::install("treeio")

#install ggmmsa from github
devtools::install_github("brj1/gghighlighter")
```

## Examples

You can simply call the `gghighlighter` function to plot a MSA.

```r
library(gghighlighter)
f <- system.file("extdata/sample.fasta", package="gghighlighter")
dev.new(width=11, height=2)
gghighlighter(f, 164, 213)

# Highlight differences
gghighlighter(f, 164, 213, consensus="PH4H_Rattus_norvegicus", remove.blank=c("-"))

# Highlight similarities
gghighlighter(f, 164, 213, consensus="PH4H_Rattus_norvegicus", remove.blank=c("-"), hightlight.diff=FALSE)
```

Or to plot DNA MSA:

```r
library(gghighlighter)
f <- system.file("extdata/dna_seq.fasta", package="gghighlighter")
dev.new(width=11, height=2)
gghighlighter(f, 0, 100, palette="Chemistry_Nucle")
```
