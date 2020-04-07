#!/bin/sh
cd Documents/4.\ Other\ projects/COVID-19/Data\ visualisation/data_scraper
Rscript 01_download-daily-PHE_data.R
git add -A
git commit -m "updated daily PHE data“
git push
echo "Task has been run :)"