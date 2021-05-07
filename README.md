# Gov 52: Models
### Yanxi Fang, Harvard University, Spring 2021

This is my final project for Gov 52 - Models (Spring 2021), in which I attempt to replicate an existing published journal article.

I chose the article ["Why Parties Displace Their Voters: Gentrification, Coalitional Change, and the Demise of Public Housing"](https://www.cambridge.org/core/journals/american-political-science-review/article/why-parties-displace-their-voters-gentrification-coalitional-change-and-the-demise-of-public-housing/DD9C48C4382889F99204E2B9191BDB24) by Winston Chou and Rafaela Dancygier, which will be published in Volume 115, Issue 2 of the *American Political Science Review* in May 2021 (the article was previously published online by Cambridge University Press on February 23, 2021). This GitHub repository contains most, but not all, of the data and code that I used for the project.

The main folder consists of several items: the readme (this file), the replication code (in R), the final replication report (code in R and output in PDF), and the BibTeX file for the bibliography. There is also a link to the "replication" folder, which is explained below.

The "replication" folder is where the working directory should be set for both of the replication R codes (from the main folder). Consequently, this folder contains all of the saved outputs from the code, such as images and .tex tables. Within this folder, there should be a link to the "data" folder; since the working directory is set to the "replication" folder, all of the code already refers to the actual data as being in the "data" subfolder.

There are also two R files in the "replication" folder; these are scripts written by Chou and Dancygier to convert the downloaded files from the UK Data Service into dataframes that are usable for the analysis. To accompany these two R files, I have provided an additional explanatory document, "Additional Info.md", to alleviate any confusion. Please note that the files from the UK Data Service are not suitable to be made public, although the Service provides data access to registered academic users at no charge. The instructions are also listed in the "Additiona Info.md" file in the "replication" folder.

Finally, all of the dataframes (mostly .csv files) are located in the "data" subfolder, which can be found within the "replication" folder.

This replication was completed using the following software versions:
- R version 4.0.3 (2020-10-10) – “Bunny-Wunnies Freak Out”
- Platform: x86_64-w64-mingw32/x64 (64-bit)
- Running under: Windows 10 Home, version 20H2

The following packages were used:
- dplyr_1.0.3
- forcats_0.5.1
- ggplot2_3.3.3
- gridExtra_2.3
- multiwayvcov_1.2.3
- plm_2.4-1
- purrr_0.3.4
- readr_1.4.0
- stargazer_5.2.2
- stringr_1.4.0
- tibble_3.0.6
- tidyr_1.1.2
- tidyverse_1.3.0
- xtable_1.8-4
- zoo_1.8-8
