# Impute.me code

The overall code is divided in two sections: one that concerns the receiving, standardization and imputation of personal genomics data. And one that is a modular setup for any derived analysis that can be performed on this standardized data. 


## Part 1: Imputation algorithm description


The imputation is performed by three functions from functions.R as well as a cron-job named imputation_cron_job.R

The three functions are

*prepare_23andme_genome* which is triggered at data-submission time (not cron-job dependent). This function simply unpacks the data, copies it to the designated imputation area (~/imputations), performs a few data consistency checks and assigns an unique id to each submission.

*run_imputation* is triggered by the cron-job checking for ready data in the imputation area (~/imputations). It consists of a series of calls to bash-based programs. First a shapeit call is made to phase the data correctly. Note that there is quite a lot of extra work involved in avoiding errors from single-sample-homozygote problems and such (up untill the cmd4 call). After shape-it calls, a call to impute2 is made and the end-product of this function is per-chromosme gen-files entitled "step_7_chr_xx", because they come from the seventh step of calculation. 

*summarize_imputation* is run immediately after run_imputation, in the cron-job, and could probably be merged into this function. The goal of this function is to organize and summarize the per-chromosome gen-files: saving as 1) a gen-file with probability-estimate-containing imputation data, and 2) a '23andme'-format file in which the calls are already made. The current threshold is 0.9, per the <a href='http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool.html'>gtools</a> default setting (but it is variable per threshold). Note that there's a lot of splitting taking place to minimize the memory footprint, which could otherwise become very large, particularly for the long chromosomes. The caller function is this script file

*imputation_cron_job.R* (a script file in the imputeme folder). This file calls the two cron-job driven functions. The extra code in the file is used to determine if it should run as a hub-job or a node-job; node jobs are prefered, because they the cron-job driven imputation functions are computationally expensive.




## Part 2: Module design description

Each module consists of a ui.R and a server.R file. The details of the setup of this can be found in the <a href='http://shiny.rstudio.com/'>R/Shiny</a> documentation. A template module which contains the very minimal configuration is found in the ['template'](https://github.com/lassefolkersen/impute-me/tree/master/template) folder.

The specific module functions are documented by their UI-provided description. For many  of them the calculations are trivial, i.e. reporting the presence and/or absence of a specific SNP. For others, we rely heavily on polygenic risk scores. Three approaches to polygenic risk scores are implemented in the function [get_GRS_2](https://github.com/lassefolkersen/impute-me/blob/5901cb626d0e50a01106d74c48540a41100974a6/functions.R#L1287):


**Basic count score**. Basically just counting the effect alleles. This is the most simple setup of polygenic risk scores. It is intuitive to understand - the more risk alleles, the higher the score. The main drawback is that it doesn't distinguish SNPs with large and small effects.

Count-score =  Σ Effect-allele-count<sub>snp</sub>


**Weighted-score**. A score that is weighted by the effect size of each SNP. This has the added benefit of weighting SNPs with large effect sizes more than SNPs with small effect sizes. Note that _beta_ is changed for _log(OR)_ as applicable for binary traits. The only draw-back of this score type is that it is on an arbitrary scale and does little to inform about risk compared to the rest of the population.

Weighted-score =  Σ Beta<sub>snp</sub> * Effect-allele-count<sub>snp</sub>


**Z-score**. A score that is given in standard-deviations above or below the average risk-score for that population. This specific implementation of the Z-score is [found here](https://github.com/lassefolkersen/impute-me/blob/5901cb626d0e50a01106d74c48540a41100974a6/functions.R#L1387-L1404). The _frequency<sub>snp</sub>_ is obtained from 1000 genomes data for the relevant super-population. _Effect-allele-count_ and _Beta_ is used as in previous scores. The _Standard-deviation<sub>population</sub>_ is calculated according to [this code](https://github.com/lassefolkersen/impute-me/blob/5901cb626d0e50a01106d74c48540a41100974a6/functions.R#L1396-L1404). In many of the modules an extra step is added where the Z-score is converted to percentage of population with lower score. This is done with the standard [pnorm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Normal.html) function, i.e. we assume everything is normally distributed. To check the validity of this assumption, some modules have an option to compare to [real distributions](https://www.impute.me/AllDiseases/).

Population-score<sub>snp</sub> = frequency<sub>snp</sub> * 2 * beta<sub>snp</sub>

Zero-centered-score =  Σ Beta<sub>snp</sub> * Effect-allele-count<sub>snp</sub> - Population-score<sub>snp</sub>

Z-score = Zero-centered-score / Standard-deviation<sub>population</sub>





## Part 3: Downloads descriptions

On each completed analysis-run an email is sent with the user's uniqueID. This uniqueID can be used to browse the above-mentioned modules. In addition three types of downloads are provided:

**23andme-format** imputed output. This format, so-called because it was introduced by the company 23andme, contains rows of all SNPs with rs-ID, chromosome, position, and genotype. When given as output from the _impute.me_-site, the only difference is that _a lot_ more SNPs are available. This is a consequence of the imputation. The file is zipped and divided by chromosome and this output format is used regardless of your input-format i.e. if you upload from 23andme, ancestry.com, or any other genetics company.

**Gen-format files** contains similar information to the 23andme-format files, i.e. all your imputed SNPs. But they do so in a more completely descriptive format, one that also reflects considerations of statistical uncertainty of genotype calls.. This format is called the [gen-format](http://www.stats.ox.ac.uk/~marchini/software/gwas/file_format.html). The key to interpretation is that each of the three right-most columns contains probabilistic information regarding your genotype. This means that a value of _A C 1 0 0_ is almost sure to be A/A, whereas _A C 0.7 0.3 0_ could be interpreted as both A/A but with some probability of being A/C.

**JSON-format files** contains calculated phenotypical data. This file contains the output of all the genetic-calculators that a sample is subjected to at _impute.me_. The JSON-format is made to be computer-readable, but many good [online viewers](http://jsonviewer.stack.hu/) exists, so you can easily inspect the data yourself. However, it is easier to browse by using the [web-interface](https://www.impute.me), so the only purpose of providing this format is long-term storage and data-interchange.