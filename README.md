# Cholinergic Pre-Phrenic Plethysmography README

# Purpose
This script will take raw plethysmography recordings from emka iox2 software and sample information from the user.\
If these plethysmography experiments contained multiple cycles in gas, it will break the file into multiple trials.\
It will use manual and automated selection criteria to isolate periods of consistent breathing, providing QC plots for each trial to ensure accuracy.\
Any measure provided by iox2 can be assessed across trials, days, and atmospheric condition per sample as both raw values and normalized to paired controls.\
Distributions plots (per breath) and box plots (per sample) visualize differences in respiration across experimental groups and conditions.

# Experimental Conditions
Paired mice are placed in emka plethysmography chambers using the iox2 software.\
These mice will be exposed to at least one trial of hypercapnia per recording.\
Changes in atmospheric conditions will be denoted by comments "Normal Air" or "CO2" in iox2.\
These comments can be added in-post to the iox.txt files if necessary.\
Each iox.txt file generated must contain the date and sample identifier in its name.

# Required Packages
stringr		v1.4.0\
chron 		v2.3.56\
ggplot2		v3.3.5\
hms		v1.1.1\
gridExtra	v2.3\
dplyr		v1.0.10\
readxl		v1.3.1\
ggpubr		v0.4.0

# Input Structures
This script requires a file containing important sample identifiers and information.\
It must be a .csv or .xslx file containing the following columns:\
	- Animal_ID - an identifer unique to each sample (such as "L32.4")\
	- Genotype - the full genotype of the sample (only for completeness)\
	- Group - a factor identifier of "wt" or "mut" to denote experimental group\
	- Sex - a factor identifier of "M" or "F" to denote sex\
	- DOB - the sample's date of birth (such as 12/5/2022)\
This file must be saved within the root folder.\
\
Within the root folder, there must be a folder titled "Raw".\
This folder contains all .iox.txt files exported from emka iox2 software for analysis.\
These files all have a predertermined structure and can be read in directly.\
If you have adjusted the output values quantified by iox2, you may need to change the script's column selection (Line 264).\
\
Optionally, you may have a file in which you manually select regions of the raw recordings to analyze.\
It must be a .csv or .xlsx file containing the following columns:\
	- File - the file which the following selection refers to\
	- Start - a time or a breath sto-id which indicates when to begin analyzing\
	- End - a time or a breath sto-id which indicates when to stop analyzing\
There can be multiple selected regions for a single file, as long as they do not overlap.\
Start and End must both be either time or breath sto-id.\
This file must be saved within the root folder.
