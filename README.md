<div align="center">

# Towards a Leptospirosis Early Warning System in North-Eastern Argentina

<div align="justify">

##### *Last update: Nov 2022*
##### *License: XXX*

## Introduction

Leptospirosis is a zoonotic disease that occurs in almost all parts of the world. Previous studies have found a link between extreme climatic events, particularly heavy rainfall and flooding, and increased risk of leptospirosis transmission. Leptospirosis is of major public health relevance in North-Eastern Argentina. 

This study aims to better characterise the effect of hydrometeorological variables on leptospirosis cases in Santa Fe and Entre Ríos, two provinces in North-Eastern Argentina. Additionally, this study aims to establish a set of predictive models with the potential of becoming part of an early warning system, based on readily available climate data, such as climate records from national meteorological services and measurements from sea surface temperature in the Pacific Ocean.

In this repository, readers will find the scripts necessary for recreating the analysis in the [paper](link). Source data are available upon request to Dr. Gabriela Muller [(email)](email). 
## Content

* [1. sourceFunctions](1_sourceFunctions.R): install packages and create functions for running models
* [2. modelFitting](2_modelFitting.R): fit all combinations of variables per province
* [3. outOfSamplePred](3_outOfSamplePred.R): compute out of sample predictions with candidate models
* [4. mainFigures](4_mainFigures.R): create figures and tables for main text
* [5. supplementaryFigures](5_supplementaryFigures.R): create figures and tables for supplementary materials
    
## Authors
##### - Martín Lotto Batista, MSc [(ORCID)](https://orcid.org/0000-0002-9437-5270)
##### - Eleanor M. Rees, PhD [(ORCID)](https://orcid.org/0000-0002-4993-2795)
##### Contact: martin.lotto@bsc.es
