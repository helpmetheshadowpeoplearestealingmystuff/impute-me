


# sudo crontab -u shiny -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputeme/extraction_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-extract-cron.log 2>&1


source("/home/ubuntu/srv/impute-me/functions.R")


crawl_for_snps_to_analyze()