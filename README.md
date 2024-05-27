# PastHumanImpact

developed by: Ondrej Mottl & Vivian A. Felde

## General

This repository is a part of the ERC HOPE project and is set to estimate if human impact altered the fundamental ecological processes.

The repository consists of modular R-script and individual functions organised by individual tasks, as well as using the {[targets](https://books.ropensci.org/targets/)} R package to create workflows for specific subroutines. Therefore, the project is divided into several steps, each step is a separate task (R scripts and/or target pipeline).

## Setup

* `R/___Init_project___.R` - set up the project on each machine. [**EACH USER SHOULD RUN THIS SCRIPT FIRST**]
* `R/00_Config_file` - The configuration file is the master file in terms of setting all criteria used throughout the repo, loading the required packages and saving settings throughout the repo.
* `R/01_run_project.R` - is the main script to run the project. It will run all the scripts in the correct order.
