rm(list = ls()) # clear global environment

library(tidyverse)

# set working directory
CurDir<-"~/Documents/current/PKU/pku_cog_functions_meta_analysis/May2022_meta-analysis_with_added_papers"
setwd(CurDir)

FunctionResultsFolder<-paste0(CurDir,"/FunctionResults2")
ForestPlotsFolder<-paste0(CurDir,"/ForestPlots2")
# set output filename for overall results summary
OverallSummaryFilename<-paste0(FunctionResultsFolder,"/OverallResultSummary.csv")
HeaderDF <- OverallSummmaryHeader<-read.csv("OverallSummaryHeader.csv")
# generates a warning because there is no data, but this is OK
write.csv(HeaderDF,OverallSummaryFilename,append=FALSE,row.names=FALSE)

# read in raw data
RawData<-read.csv("DataForMetaAnalysis.csv")
FunctionOrderInformation<-read.csv("SummaryTableFunctionOrder.csv")

RawData$StudyNameFactor<-as.factor(RawData$StudyName)
RawData$FunctionFactor<-as.factor(RawData$Function)
RawData$TaskNameFactor<-as.factor(RawData$ShortTitle)
RawData$StudyFactor <- as.factor(RawData$StudyFactor)

# done with preliminaries - now we can do analyses for separate functions
####################################################################


# Flexibility

FunctionID <- "Flexibility"   # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Flexibility" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################
UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES<- -5 
MaxES<- 2

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))



# Reasoning

FunctionID <- "Reasoning "   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Reasoning" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES<- -7 
MaxES<- 2

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Inhibition

FunctionID <- "Inhibition"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Inhibition" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES<- -4 
MaxES<- 3.2

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Sustained attention

FunctionID <- "Sustained attention"   # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Sustained attention" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 0
# different parts of the forest plot
PlotPercentage<- .35  # percentage of space devoted to forest plot (vs text)
StudyNamePercent<-0.25 # percent of _text_ area for study name
MeanPercent<-0.09 # width of columns for means - % of total _text_ area
SDPercent<-0.065 # width of column for SD
NPercent<-0.04 # width of column for N
WtPercent<-0.04

UsePlotRangeDefaults <- 0
MinES<- -7 
MaxES<- 2

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))


# Higher Language Skills

FunctionID <- "Higher Language Skills"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Higher language skills" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Verbal STM/WM

FunctionID <- "Verbal STM/WM"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Verbal STM/WM" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))


# Verbal STM without WM

FunctionID <- "Verbal STM/WM"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Verbal STM/WM-w/o WM" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID) 
# take out all results that include working memory - Pilotto could be back + forward
MyData <- MyData[MyData$StudyName != "Pilotto et al 2021",]
MyData <- MyData[MyData$StudyName != "Abgottspon et al. ",] 
MyData <- MyData[(MyData$StudyName != "Aitkenhead et al 2021") | MyData$ShortTitle != "Digit backwards (from WAIS IV)",] 
MyData <- MyData[MyData$StudyName != "Channon et al 2005",]
MyData <- MyData[MyData$StudyName != "Channon et al 2007",]

FunctionFilename<-gsub("/","_",unique(MyData$Function))
FunctionFilename <- paste0(FunctionFilename,"_without_WM")
# this will be used for plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))


# Visual STM

FunctionID <- "Visual STM"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visual STM" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Verbal learning LTM

FunctionID <- "Verbal learning LTM"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Verbal learning/LTM" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Visual learning LTM

FunctionID <- "Visual learning LTM"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visual learning/LTM" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES<- -5
MaxES<- 2

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Visuo-spatial skills

FunctionID <- "Visuo-spatial skills"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visuo-spatial skills" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Simple RT-Visuo-spatial

FunctionID <- "Simple RT-Visuo-spatial"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Simple RT: Visuo-spatial" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 0
MinES<- -6
MaxES<- 3

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Visual spatial attention RT

FunctionID <- "Visual spatial attention RT"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visual spatial attention RT" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Visuo-spatial attention acc

FunctionID <- "Visuo-spatial attention acc"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visuo-spatial attention: accuracy" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Visuo motor control

FunctionID <- "Visuo motor control"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Visuo motor control" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Naming RT

FunctionID <- "Naming RT"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Naming RT" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Language Accuracy

FunctionID <- "Language Accuracy"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Language accuracy" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("  ","_",gsub("/","_",unique(MyData$Function)))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Spelling

FunctionID <- "Spelling"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Spelling" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Phonological Tasks

FunctionID <- "Phonological Tasks"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Phonological tasks" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

# Social Cognition

FunctionID <- "Social Cognition"   # note extra space # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "Social cognition" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 1
UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))


# IQ

FunctionID <- "IQ"   # this sets the function we will run the analysis on
# it should match the "Function" column variable from the input data frame
FunctionTitle <- "IQ" # This will be used to label the plot, so could be different from FunctionID
####################### SET THE OUTPUT FILENAME FOR THE PLOT HERE ##################

UsePlotPercentDefaults <- 0
PlotPercentage<- .3  # percentage of space devoted to forest plot (vs text)
StudyNamePercent<-0.22 # percent of _text_ area for study name
MeanPercent<-0.09 # width of columns for means - % of total _text_ area
SDPercent<-0.065 # width of column for SD
NPercent<-0.04 # width of column for N
WtPercent<-0.04

UsePlotRangeDefaults <- 1

MyData<-RawData %>% filter(Function == FunctionID)
FunctionFilename<-gsub("/","_",unique(MyData$Function))
# this will be used to label the plot and report file output.

rmarkdown::render(input = "MetaAnalysisPKUv2_OneFunction2.Rmd", 
                                    output_format = "html_document",
                                    output_file = paste0(FunctionResultsFolder,"/",FunctionFilename,"_meta-analysis_report.html"))

