#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# See https://books.ropensci.org/targets/hpc.html


# run target pipeline
targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint


# inspect output
targets::tar_read(data)

# check workflow
targets::tar_visnetwork()


# commit code to github examples
gert::git_init()
gert::git_add("_targets.R")
gert::git_commit("Building up data derivatives for H1")

# snapshot of the data, but first check that code is up to date
targets::tar_git_status()

# initialize the data repository
targets::tar_git_init()

# take a snapshot of target data
targets::tar_git_snapshot()

# repeat in cycle when working with it

# view log
targets::tar_git_log()


# to check out older code or whatever
gert::git_branch_checkout("fix_data_storage")

# check out data
# every data snapshot with tar_git_snapshot() creates a new Git branch
targets::tar_git_checkout()
