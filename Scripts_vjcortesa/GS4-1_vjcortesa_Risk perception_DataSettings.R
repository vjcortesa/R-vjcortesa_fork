# Step 1: Data Settings ------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
library(readxl)
# Load if using RStudio (interactive session)
library(rstudioapi)

# Get the path of the current script (works in RStudio)
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/GS4_vjcortesa_...R"
script_path <- rstudioapi::getActiveDocumentContext()$path
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/"
scriptfolder_path <- dirname(script_path)
# Set path to shared folder with the rawdata one directory up
datasetfolder_path <- paste0(dirname(scriptfolder_path),"/Datasets/")

# Allow to set and check working space for the github collaborator coding
setwd(scriptfolder_path)
getwd()
dir()
# Set paths to datasets
dataset_settings_path <- "GS4_vjcortesa_How is risk perception assessed_questions overview.xlsx" 
dataset_240924_path <- paste0(datasetfolder_path,"housinggame_session_16_240924_EPA_IntroDays_Ommen_surveys/241108_240924_WhereWeMove sessions - presurvey.xlsx")
dataset_250923_path <- paste0(datasetfolder_path,"housinggame_session_19_250923_EPA_IntroDays_Overasselt_surveys/251119_250923_WhereWeMove sessions - presurvey.xlsx")
dataset_251007_path <- paste0(datasetfolder_path,"housinggame_session_20_251007_VerzekeraarsMasterClass_surveys/251009_251007_WhereWeMove sessions - presurvey.xlsx")

# Read datasets
df_RP_settings <- read_excel(dataset_settings_path, sheet = "dataset_240924")
df_RP_dataset_240924 <- read_excel(dataset_240924_path)
df_RP_dataset_250923 <- read_excel(dataset_250923_path)
df_RP_dataset_251007 <- read_excel(dataset_251007_path)

# Same headers for all datasets
df_RP_headers <- names(read_excel(dataset_settings_path, sheet = "dataset_240924", n_max = 1))

df_RP_settings_240924 <- data.frame(
  RiskPerceptionQuestion = df_RP_settings[,1],
  QuestionAlias = df_RP_settings[,2],
  QuestionText = df_RP_settings[,3]
  ) 
