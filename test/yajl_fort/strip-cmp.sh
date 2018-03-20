#!/bin/bash
# $1 = strip executable
# $2 = JSON input file

$1 $2 > out1.json
$1 out1.json > out2.json
cmp -s out1.json out2.json
