#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# load dependencies
import csv
import json

# load json into dictionary in memory
with open("../out/pond_comparison.json","r") as f:
    hold = json.load(f)

# manipulate dictionary to obtain relevant values
rel = {}
for key in hold.keys():
    if hold[key]["IDDevice"] in ["SAM_8623","SAM_8624","SAM_8622"]:
        rel[key] = hold[key]

# get information out of rel
final = [[rel[key]["IDDevice"],rel[key]["DateTime"]]+el for key in rel.keys() for el in rel[key]["data"]]

# write to output file
with open("../out/sample.csv","w") as f:
    w = csv.writer(f)
    w.writerows(final)
