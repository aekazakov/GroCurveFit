#!/bin/bash
script_dir=$(dirname "$(readlink -f "$0")")
export R_LIBS=/kb/deployment/lib:
Rscript $script_dir/../lib/GroCurveFit/GroCurveFitServer.r $1 $2 $3
