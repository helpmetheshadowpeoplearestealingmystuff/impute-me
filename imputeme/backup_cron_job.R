

# sudo crontab -u shiny -e
# 10 20 * * * /srv/shiny-server/gene-surfer/imputeme/backup_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-backup-cron.log 2>&1


rsync -r -h -t --progress /home/ubuntu/data/ lasfol@computerome.cbs.dtu.dk:/home/people/lasfol/dataBulk/Miscellanous/2015-10-05_impute_me_backup/
