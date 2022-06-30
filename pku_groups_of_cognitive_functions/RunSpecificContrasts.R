rm(list = ls()) # clear global environment

CurDir<-"/Users/Olsonac-admin/Documents/current/PKU/pku_function_groups"
setwd(CurDir)

FunctionResultsFolder<-paste0(CurDir,"/MetaAnalysisResults")
ForestPlotsFolder<-paste0(CurDir,"/ForestPlots")

# Language vs Executive function - plot individual functions 

GroupInfoFile<-"EF_lang_individual_functions_StudyList.csv"
FunctionFilename<-"EFvsLang_IndividualFunctions"
FunctionTitle<-"Executive function vs. language: Individual functions"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES <- -5 
MaxES <- 3


rmarkdown::render(input = "MetaAnalysisPKUv2_GroupsOfFunctions.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_meta-analysis_report.html"))


# Language vs Executive function - general contrast

GroupInfoFile<-"EF_lang_EFvsLang_StudyList.csv"
FunctionFilename<-"EFVsLang"
FunctionTitle<-"Executive function vs. language: overall"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES <- -5 
MaxES <- 3

rmarkdown::render(input = "MetaAnalysisPKUv2_GroupsOfFunctions.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_meta-analysis_report.html"))


# Accuracy vs reaction time

GroupInfoFile<-"Accuracy_vs_ReactionTime_StudyList.csv"
FunctionFilename<-"AccuracyVsReactionTime"
FunctionTitle<-"Accuracy vs reaction time"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES <- -3 
MaxES <- 2

rmarkdown::render(input = "MetaAnalysisPKUv2_GroupsOfFunctions.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_meta-analysis_report.html"))


# visual vs verbal

GroupInfoFile<-"Visual_vs_Verbal_StudyList.csv"
FunctionFilename<-"VisualVsVerbal"
FunctionTitle<-"Visual vs verbal tasks"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES <- -5 
MaxES <- 3

rmarkdown::render(input = "MetaAnalysisPKUv2_GroupsOfFunctions.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_meta-analysis_report.html"))

# Naming RT vs visuo-spatial RT

GroupInfoFile<-"Naming_vs_VisuoSpatial_RT_StudyList.csv"
FunctionFilename<-"NamingVsVisuoSpatialRT"
FunctionTitle<-"Naming RT vs. Visuo-spatial RT"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1
#MinES <- -5 
#MaxES <- 3

rmarkdown::render(input = "MetaAnalysisPKUv2_GroupsOfFunctions.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_meta-analysis_report.html"))


