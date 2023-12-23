# Hardware Hash Functions


This repo contains two main components, the source code used to investigate our
topic and the final paper describing our results.  

## Source Code 

Inside src/, there are two subdirectories, one for each approach we
investigated, logical synthesis and voting theory.  

### Logical Synthesis 

[See the readme here](/src/synthesis/README.md).

### Voting Theory

The second approach is based on parametrized hashing under the guise of a voting
scheme.  It is written in Python and may be run as:

`python weighted_pearson.py`

This code depends on numpy and matplotlib, which may be installed with the pip
tool, or however your distro handles python packages.

## Final Paper

The final paper is included in the report/ directory as a pdf with instructions
for building it from scratch. You can also download it from [here](https://github.com/atefehmohseni/hardware_hash_functions/blob/main/report/Hardware_Hash_Functions.pdf).
