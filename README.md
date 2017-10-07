# Impute.me code

The overall code is divided in two sections: one that concerns the receiving, standardization and imputation of personal genomics data. And one that is a modular setup for any derived analysis that can be performed on this standardized data. 


## Part 1: Imputation algorithm description


The imputation is performed by three functions from functions.R as well as a cron-job named imputation_cron_job.R

The three functions are

*prepare_23andme_genome* which is triggered at data-submission time (not cron-job dependent). This function simply unpacks the data, copies it to the designated imputation area (~/imputations), performs a few data consistency checks and assigns an unique id to each submission.

*run_imputation* is triggered by the cron-job checking for ready data in the imputation area (~/imputations). It consists of a series of calls to bash-based programs. First a shapeit call is made to phase the data correctly. Note that there is quite a lot of extra work involved in avoiding errors from single-sample-homozygote problems and such (up untill the cmd4 call). After shape-it calls, a call to impute2 is made and the end-product of this function is per-chromosme gen-files entitled "step_7_chr_xx", because they come from the seventh step of calculation. 

*summarize_imputation* is run immediately after run_imputation, in the cron-job, and could probably be merged into this function. The goal of this function is to organize and summarize the per-chromosome gen-files: saving as 1) a gen-file with probability-estimate-containing imputation data, and 2) a '23andme'-format file in which the calls are already made. The current threshold is 0.9, per the <a href='http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool.html'>gtools</a> default setting (but it is variable per threshold). Note that there's a lot of splitting taking place to minimize the memory footprint, which could otherwise become very large, particularly for the long chromosomes. The caller function is this script file

*imputation_cron_job.R* (a script file in the imputeme folder). This file calls the two cron-job driven functions. The extra code in the file is used to determine if it should run as a hub-job or a node-job; node jobs are prefered, because they the cron-job driven imputation functions are computationally expensive.




## Part 2: Module design description.

Each module consists of a ui.R and a server.R file. The details of the setup of this can be found in the <a href='http://shiny.rstudio.com/'>R/Shiny</a> documentation. A template module which contains the very minimal configuration is found in the 'template' folder.

The specific module functions are documented by their UI-provided description.
