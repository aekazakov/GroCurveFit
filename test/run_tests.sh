#!/bin/bash
script_dir=$(dirname "$(readlink -f "$0")")
export KB_DEPLOYMENT_CONFIG=$script_dir/../deploy.cfg
export KB_AUTH_TOKEN=`cat /kb/module/work/token`
export R_LIBS=/kb/deployment/lib:
cd $script_dir/..
R -q -e "for(infile in dir(\"test\", pattern=\"_test\\\\.r$\")) {source(paste(\"test\",infile,sep=\"/\"))}"
