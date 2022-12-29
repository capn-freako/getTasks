#!/usr/bin/env bash

# Gitit action item updater.
#
# David Banas
# Feb. 22, 2022

cd /c/Users/capnf/Documents/gitit/wikidata
getTasks . >currentTasks.page
git add currentTasks.page
git commit -m 'Updated currentTasks.page.'
