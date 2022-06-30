rm(list = ls()) # clear global environment

CurDir<-"/Users/Olsonac-admin/Documents/current/PKU/pku_function_groups"
setwd(CurDir)

FunctionResultsFolder<-paste0(CurDir,"/MetaAnalysisResults")
ForestPlotsFolder<-paste0(CurDir,"/ForestPlots")

# Executive function, no speed. 

GroupInfoFile<-"EF_no_speed_StudyList.csv"
FunctionFilename<-"EF_no_RT"
FunctionTitle<-"Executive function without RT tasks"
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES <- -5 
MaxES <- 3


rmarkdown::render(input = "MetaAnalysisPKUv2_SingleFunctionGroup.Rmd", 
                  output_format = "html_document",
                  output_file = paste0("MetaAnalysisResults/",FunctionFilename,"_single_function_meta-analysis_report.html"))
