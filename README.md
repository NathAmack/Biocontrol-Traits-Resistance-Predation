# Project

Version 0

A small trial to compare the growth of a subset of protist species on different bacterial isolates.

Open Trial (RProject) and then the file mainAmack.R

**Requirements**
The project is run on R version 3.4.3 (2017-11-30)
Platform: i386-w64-mingw32/i386 (32-bit)
Running under: Windows >= 8 x64 (build 9200)
Requires to install.packages("ggplot2"), install.packages("gplots"), install;packages("RColorBrewer") and install.packages("nlme"). 

**Description of the data**
* We have 3 different time points: day 1, day 3 and day 5.
* The raw data contains two media: 2%KB and PAS. We focus on 2%KB.
* The raw data also contains protist cyst (*i.e.* dormant stage) counts in addition to the active individuals, we focus on the active individuals.
  
**Description of the analysis**
* We want to have a descriptive analysis and plot the data in a boxplot.
* We also want to go through a statistical analysis to test if there is any differences between the groups. To do so, we test different models. 
  * A first approach is to use a model based on a generalized least square to check assumptions. Using plot allows to visually check for heterosedasticity. If heterosedasticity is observed, we can run a second model based on gls but allowing for different variance for the groups. We can compare these two models using anova().
  * A second approach is to use glm() and a poisson distribution because our data are working with counts data. In case we expect overdispersion (*i.e.* the variance is larger than the mean), we can correct the standrad errors using a quasi-GLM model (p.226, Zuur *et al.* book). We can comparte these two models using anova() with test="Chi". 
* We then create a heatmap to have an overview of all interactions 


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
