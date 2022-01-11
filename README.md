# REal World Assessment of Drug (REWARD) performance Execution Package

**This Package is under heavy development and is not intended for usage at this stage.**

This package represents the implementation of REWARD, a toolset built on top of OHDSI HADES library
of standard methods for observational healthcare research.

The objective of REWARD is to provide statistically rigorous, population level effect estimates from
observational data sets as rapidly as possible.
This is achieved through the combination of data from as many participating sites as possible.
The primary mode of operation is hypothesis generation about existing medications.

## Requirements
An OMOP common data model (CDM) compliant database version 5.3 or higher.

## Installation

We recommend that you use `renv`:

```
install.packages("renv")
renv::restore()
```

## Usage
First, create a configuration file for your CDM.
```r
createCdmConfiguration("myCdmConfig.yml")
```

Now we want to run a task that will create cohorts and run the SCC analysis,
we run this as a background task in R studio:
```r

```
This process will take several hours to run (depending on your infrastructure), 
so plan this job accordingly.
Note that this job can resume (to some extent) from crashing;
cohorts will only be generated once and intermediate SCC results will be stored.

## Note on data sharing
For the purposes of transparency, all data that is shared in REWARD is transferred in CSV format.
These are considered uncalibrated, blind effect estimates.
We note that viewing data in this package is not possible.
To view results the REWARD Shiny applications and results store should be used.
However, we note this should be done in a principled manner.
Unblinding results without first selecting negative controls and observing the level of bias
an effect estimate may have will ultimately lead to bias.

