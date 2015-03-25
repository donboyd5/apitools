# RPackageCreationforbtools.r
# Don Boyd
# 3/25/2015

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# http://www.molecularecologist.com/2013/11/using-github-with-r-and-rstudio/
# https://www.rstudio.com/ide/docs/version_control/overview
# http://kbroman.github.io/github_tutorial/


# http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# http://adv-r.had.co.nz/Package-basics.html
# https://github.com/klutometis/roxygen#roxygen2

# http://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages
# https://sites.google.com/site/hackoutwiki/developers-corner/developing-r-packages

# The following SSH key was added to your account:
#
#   GitHub for Windows - Don-PC
# 75:4b:6d:82:41:88:79:28:e1:b2:7a:01:66:93:61:8f
#
# If you believe this key was added in error, you can remove the key and disable
# access at the following location:
#
#   https://github.com/settings/ssh

getOption("defaultPackages")

# Once the package is in full gear, here are the basic steps, DETAILED below
# library(devtools)
# library(roxygen2)
document()
install("apitools")
# then commit and push

# Steps for creating, revising, documenting, installing, and uploading a package to github:
# 0.a Load needed packages
#       library(devtools)
#       library(roxygen2)  # make sure latest from github is installed:  devtools::install_github("yihui/roxygen2")
# 0.b Create the package directory if this is a new package:
#       create("apitools")
# 1. Revise/add functions and documentation, etc. - see examples below
# 2. Create markup documentation for the revised package via:
#       document()
# 3. Install the package locally
#       setwd("..") # maybe not needed
#       install("apitools")
# 4. Commit changes locally to git
#       - select commit from the git button on toolbar above
#       - press commit to commit the changes to the local repository (git on my computer)
# 5. Upload revised package to github (see http://r-pkgs.had.co.nz/git.html#git)
#       - create a new repo on github if not already done:
#           go to: https://github.com/new
#           create a repo with the same name as the package (e.g., apitools)
#       - initialize the repo
#           open the git shell from within RStudio
#           push the repo from the PC to the web with these two commands:
#             git remote add origin https://github.com/donboyd5/apitools.git
#             git push -u origin master
#       - after this initialization, do pushes from within RStudio
#       - uid id donboyd5, pw is the 8 chars pw
# 5. Optionally install from github
#       devtools::install_github("donboyd5/apitools")
# 6. Due to apparent RStudio bug, best to exit RStudio and then re-enter to see the documentation for the revised package


# # remove.packages("apitools")
#
#
# # here is an example of how to document a function
#
# #' @title Capitalize first letter of each word
# #'
# #' @description \code{capwords} capitalize first letter of each word
# #' @usage capwords(s)
# #' @param s The string to capitalize words of
# #' @details All white space is removed from the trailing (right) side of the string.
# #' @return The initial-capped result.
# #' @keywords capwords
# #' @export
# #' @examples
# #' capwords("string to capitalize words in")
#




