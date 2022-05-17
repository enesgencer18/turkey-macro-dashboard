#!/usr/bin/env bash

Rscript -e "source('./data-refresh/data_refresh.R')" 

git config --global --add safe.directory /__w/turkey-macro-dashboard/turkey-macro-dashboard
git config --local user.email "actions@github.com"
git config --local user.name "GitHub Actions"
git add --all
git commit -am "add data"
git push 
