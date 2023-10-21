#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: R Profile 
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------

# set working directories and all directories 
# this is the profile for the Non-Cognitive Skills research project
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#version from everyone, the profile works for everyone

if (USERNAME == "kavanesyan"){
  projectFolder  <- getwd()
} 

if (USER == "karavan88"){
  projectFolder  <- getwd()
} 


# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

# set up key folders
inputData <-    file.path(projectFolder, "01_input_data")
ssesData  <-    file.path(inputData, "oecd_sses_2021") 
rcodes    <-    file.path(projectFolder, "02_code")

