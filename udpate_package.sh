#!/bin/bash

# re-create documentation
Rscript update_package.R

# add all new things & commit
git add *
git commit -m 	'$@'

# push to github
#gpush