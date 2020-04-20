#!/bin/bash

PATH=/usr/local/bin:/usr/local/sbin:~/bin:/usr/bin:/bin:/usr/sbin:/sbin
cd Documents/4_Projects/COVID-19/data_scraper

mkdir -p public

python3 getdata.py build > ./data/original/coronavirus-cases.csv
python3 getdata_deaths.py build > ./data/original/coronavirus-deaths.csv

Rscript 01_download-daily-PHE_data.R
Rscript 02_download-daily-JHU_data.R
Rscript 03_download-daily-brazil_data.R

git add -A
git commit -m “scrape”
git push origin master
echo “Data scraped”

cd public
echo "<h1>Coronavirus data</h1>" > index.html
cat >>index.html <<EOF
<p>
This is a listing of JSON data files from the gov.uk <a href="https://coronavirus.data.gov.uk/">Coronavirus Dashboard</a>
</p>
<p>
The most recent JSON is also converted to a CSV and put here.
</p>
<p>Code for this is in <a href="https://gitlab.com/b-rowlingson/covidscrape">covidscrape</a> in gitlab. I make no
promises of correctness or timeliness of this listing. If this is not up to date then download the repository code and
run the build script.
</p>
EOF
echo "<h2>CSV</h2>" >> index.html
echo "<ul>" >>index.html
for f in *.csv ; do
    echo '<li><a href="'$f'">'$f'</a></li>' >> index.html
done
echo "</ul>" >> index.html

echo "<h2>JSON</h2>" >>index.html
echo "<ul>" >>index.html
for f in *.json ; do
    echo '<li><a href="'$f'">'$f'</a></li>' >> index.html
done
echo "</ul>" >> index.html
