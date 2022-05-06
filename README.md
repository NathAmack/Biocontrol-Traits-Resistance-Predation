# Project

Version 0

You can find here the different R scripts used for the data analysis done in the research project published in 2020 in Frontiers in Microbiology (doi: 10.3389fmicb.2020.614194)
Note that the phylogenetic analyses described in the paper were done by the co-authors and are thus not given here.

We studied the ability of 6 different protist species to grow on different bacteria (7 Pseudomonas spp. and *Escherichia coli*) in an *in vitro* assay (randomly set in 7 96-wells plates). We investigated the relative importance of different bacterial traits previously characterized and related to biocontrol ability, plant growth promotion, and (root) colonization.

**Requirements**
The project is run on R version 3.4.3 (2017-11-30)
Platform: i386-w64-mingw32/i386 (32-bit)
Running under: Windows >= 8 x64 (build 9200)
Packages requirements are given in the beginning of each script.

**Description of the data**

* We have the estimated densities of the protist population at 3 different time points: day 1, day 3 and day 5.
* The raw data contains two media: 2%KB and PAS. We focus on 2%KB in most data analysis.
* The raw data also contains protist cyst (*i.e.* dormant stage) counts in addition to the active individuals, we focus on the active individuals.
* Bacterial traits and related indexes of the Pseudomonas spp. as characterized by from Agaras et al. 2015 (doi: 10.1016/j.biocontrol.2015.07.003) and Agaras et al.   2018 (doi: 10.1371/journal.pone.0194088); The indexes proposed by Agaras et al. (2015) are: the Biocontrol Potential Index (BPI, e.g., antibiotic genes, HCN         production, lytic enzymes) and the Direct Growth Promotion Index (DGPI, e.g., P solubilization, IAA, ACC deaminase). The Colonization Potential Index (CPI) was     constructed considering motility, quorum sensing, and biofilm activities separately from the rest.

! If no growth of protists was observed in PAS AND in 2%KB, the treatment is removed from the dataset. This was only the case for protists grown on RBAN4. We removed RBAN4 from the study. In addition, RBAN4 showed opposite pattern compared to the previous setup. The other protists showed growth in either PAS or 2%KB. Some did only grow in 2%KB, suggesting an important role of bottom up control. 
  
**Description of the analysis**
* First, we investigated the growth of protists on the different bacterial isolates in 2% KB;   Rcode named as "ProtistDensityCombined.R"
    *   we investigated the growth of all protists combined on the different bacterial isolates in 2% KB using a zero-inflated model hurdle or two-part for our             data (Zuur et al., 2009, doi: 10.1007/978-0-387-87458-6) to explain the achieved protist densities with bacterial isolates as explanatory variables 
    *   We also investigated the growth of each protist separately on each bacterial isolate.using an ANOVA analysis (stats:lm and base:summary) on the square               root transformed data, using bacterial isolates as explanatory variable for the protist density at day 3 in 2% KB
    *   We computed a heatmap to show the protist density of each species in co-culture with each bacterial isolate (gplots:heatmap; Warnes et al., 2016;                   https://github.com/talgalili/gplots (accessed January, 2020).

* Second, we investigated the effect of protists on the bacterial density using as proxy the OD600
    *   We plotted the treatment mean of bacterial density against the treatment mean of protist density [log10(active cells cm^2 + 1); addition of a one because
        of the presence of zeros] at day 3 in 2% KB and computed a Spearman rank correlation (stats:cor.test).
    *   To further investigate this relationship, we ran an ANOVA analysis (stats:lm and base:summary) using protist isolates as explanatory variable for
        the bacterial density (OD600 values) at day 3 in 2% KB. We ran the analysis separately for each bacterial isolate. 
    *   We computed a heatmap to show the bacterial density of each isolate exposed to each predator protist (gplots:heatmap; Warnes et al., 2016).

* Third, we investigated correlations between the protist density and specific bacterial traits.
    *   We used Spearman rank correlations (stats:cor.test) to analyze the relation between protist density and the bacterial traits with counts or continuous data
    *   We performed point-biserial correlations (ltm:biserial.cor; Rizopoulos, 2006; doi: 10.18637/jss.v017.i05) to study the relation between the protist density         and dichotomous data of the bacterial traits
    *   All correlations were combined into one correlation matrix (corrplot:corrplot; Wei and Simko, 2017; https://github.com/taiyun/corrplot (accessed January,           2020).
    *   We computed the statistical significance tests using stats:cor.test specifying the method to be Spearman or Pearson for the point-biserial correlation.
    *   Using the Spearman rank correlation (stats:cor.test), we also investigated the correlation between the combined densities of all protist isolates at day 3,         in 2%KB, and plant-beneficial related indexes proposed by Agaras et al. (2015), as well as the correlation between the combined bacterial densities at day 3         in 2%KB, and the indexes
    *   The results were displayed using corrplot:corrplot.


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
