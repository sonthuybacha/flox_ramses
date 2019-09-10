#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# load dependencies
import re
import json
import argparse
import numpy as np
from tqdm import tqdm

##############################
# define main function
##############################

def convertJSON(infile,outfile="out"):
    # initialize variables
    json_dict = {}
    data = False
    # find total lines in file for tqdm bar
    # adapted from https://stackoverflow.com/a/55188797
    num_lines = np.sum([1 for line in open("./data/ramses/"+infile,"r")])
    # start readings lines and sequentially parsing
    with open("./data/ramses/"+infile,"r") as f:
        for line in tqdm(f,total=num_lines):
            if len(re.findall("IDData\s+", line)) == 1:
                # trigger to create new tree branch
                el = re.split("\s*=\s*",line.strip())
                json_dict[el[1]] = {}
                reserve = el[1]
            elif "=" in line and "Version" not in line:
                # trigger to add leaves
                el = re.split("\s*=\s*",line.strip())
                json_dict[reserve][el[0]] = el[1]
            elif "[END]" not in line and "[DATA]" in line:
                # trigger to create data branch and accumulate intermediate list
                data = True
                int_ls = []
            elif "[END]" in line and "[DATA]" in line:
                # trigger to stop accumulation and deposit list
                data = False
                json_dict[reserve]["data"] = int_ls  
            elif data:
                # trigger to append data lines to intermediate list
                int_ls.append(re.split("\s+",line.strip()))
    # write parsed dictionary to json
    with open("./out/"+outfile+".json", "w") as f:
        json.dump(json_dict,f)

##############################
# main command call
##############################

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=str, default = "out",
                        help="name of output json <default:'out'>")
    requiredNamed = parser.add_argument_group('required named arguments')
    requiredNamed.add_argument('-i', '--input', type=str,
                               help="name of input file, eg. 'ramses.dat'",
                               required=True)
    args = parser.parse_args()
    convertJSON(args.input,args.out)
