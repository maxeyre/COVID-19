#!/bin/sh
PATH=/usr/local/bin:/usr/local/sbin:~/bin:/usr/bin:/bin:/usr/sbin:/sbin
cd Documents/4_Projects/COVID-19/data_scraper
Rscript 01_download-daily-PHE_data.R
Rscript 02_download-daily-JHU_data.R
git add -A
git commit -m "updated daily PHE data“
git push
echo "Task has been run :)"