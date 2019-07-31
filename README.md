## provExplainR
Reads two provenance directories and generates difference between
two versions including the environment in which the scripts were executed,
versions of attached libraries, versions of provenance tool, name and content
of main and sourced scripts.

provExplainR works with provenance collected by the rdt or rdtLite packages.

## Installation
Installation from GitHub:

```r
# install.packages("devtools")
devtools::install_github("End-to-end-provenance/provExplainR")
```
Once installed, load the package:

```{r}
library("provExplainR")
```

## Usage

1. To view differences between two provenance directories:

```
prov.explain (olderProv.dir = "prov_testdata_2019-06-10T14.35.52EDT", newerProv.dir = "prov_testdata_2019-06-17T16.20.23EDT")
```

prov.explain function has one optional parameters, <i>save</i>. 
If <i>save</i> is true, the comparison result is saved to a file, in addition to
being displayed in the console. The file is named <i>prov-explain.txt</i> and 
is stored in the newer provenance directory among two given directories. 
The default value of <i>save</i> is false.

2. To view the difference between two scripts in the old and new provenance directories:

```
prov.diff.script (first.script = "mainScript.R", olderProv.dir = olderProv."prov_testdata_2019-06-10T14.35.52EDT", newerProv.dir =  "prov_testdata_2019-06-17T16.20.23EDT")
```

prov.diff.script has one optional parameters, <i>second.script</i>.
If <i>second.script</i> is specified, prov.diff.script assumes the first script 
argument is the name of the script located in the older provenance directory, 
and the second script argument is the name of the script located in the newer 
provenance directory. This can be helpful in such cases as main or sourced scripts
in old and new provenance got renamed. If <i>second.script</i> is not specified,
prov.diff.script assumes that you want to view difference between two scripts with
the same name in both old and new provenance directory.
The default value of <i>second.script</i> is NULL. 

## Example

Here is an example of what the comparison result looks like. provExplainR first looks
at environment factors (like architecture, operating systems, scriptTimestamp, etc.),
then versions of attached libraries, versions of provenance tool rdtLite, name and 
content of main and sourced scripts. 

```
ENVIRONMENT CHANGES: 
Environment updates:
Environment factor: scriptTimeStamp
### Old value: 2019-06-10T14.33.23EDT
### New value: 2019-06-10T15.05.25EDT
Environment factor: provDirectory
### Old value: /Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data_2019-06-10T14.35.18EDT
### New value: /Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data_2019-06-26T15.41.09EDT
Environment factor: provTimestamp
### Old value: 2019-06-10T14.35.18EDT
### New value: 2019-06-26T15.41.09EDT

New environment factors added:
            label value
 totalElapsedTime 5.562


LIBRARY CHANGES: 
Library updates:
    name old.version new.version
 rdtLite       1.0.2       1.1.0


Libraries added:
       name version
 provParseR   0.1.2


Libraries unloaded:
           name version
 provSummarizeR     1.0


PROVENANCE TOOL CHANGES: 
Tool updates: 
Tool name: rdtLite
### Old tool version: 1.0.2 ; old json version: 2.1
### New tool version: 1.1.0 ; new json version: 2.2

SCRIPT CHANGES: 
The content of the main script HF-data.R has changed
### Old script HF-data.R was last modified at: 2019-06-10T14.33.23EDT
### New script HF-data.R was last modified at: 2019-06-10T15.05.25EDT
```