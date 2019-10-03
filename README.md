## flox_ramses

A joint project to organize and calculate key remote sensing output metrics. Input data was collected during field-work and the next task involves filtering and calculating key optical coefficients.

In order to use this repository for testing and submitting pull requests, it is advised to enable a pre-commit hook which keeps python dependencies in `requirements.txt` up-to-date.

```shell
$ ./init.sh
```

### 1. Aggregating flox data

In order to aggregate raw flox data into a csv file, place the relevant flox data inside `./data/flox` and execute the following:

```shell
$ Rscript aggregate_flox.R
```

The corresponding output will be saved in the `./out` directory.

### 2. Parsing RAMSES data and writing to JSON

The script `dat2json.py` parses a RAMSES text output file into a python dictionary and correspondingly writes it to the `./out` directory as a JSON file.

```
usage: dat2json.py [-h] [--out OUT] -i INPUT

optional arguments:
  -h, --help            show this help message and exit
  --out OUT             name of output json <default:'out'>

required named arguments:
  -i INPUT, --input INPUT
                        name of input file, eg. 'ramses.dat'
```

### 3. Visualization of pond-based results

`figure_vis.R` contains a workflow to plot pond-based results after preprocessing a json file from `dat2json.py`.

`temporal_slice_vis.R` contains a workflow to plot pond-based results after preprocessing based on temporal slices.

### 4. Changelog

Proposed changes to our workflow are highlighted in `todos.md`.

### Authors

Atreya Shankar, Remika Gupana
