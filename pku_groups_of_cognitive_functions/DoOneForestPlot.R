DoOneForestPlot<-function(FPData,RandomTermString,FunctionTitle,PlotFilename,UsePlotRangeDefaults,UsePlotPercentDefaults){
  
  # this step does the statistical model for the meta-analysis
  # g is the effect sizes
  # Vg is the variance of the effect sizes
  # random is what describes the relationship between numbers and studies
  # 1 | StudyNum / ObsNum means that ObsNum (different for each measure) is nested
  # within StudyNum.  "1 |" is needed because this is how random intercepts are
  # described when specifying a mixed-effect model.  1 means intercept and what follows
  # the | is what the intercept is for. 1 | StudyNum / ObsNum means a random intercept for
  # ObsNum nested within StudyNum
  FPData<-FPData[order(FPData$StudyName,FPData$ShortTitle),] # make sure that model results and table results (below)
  # are in the same order
  
  HetResult<-metahet(FPData$g,FPData$Vg)
  ISqr<-HetResult$I2
  Q<-HetResult$Q
  TauSqr<-HetResult$tau2.DL
  
  # typical RandomTermString "~ 1 | StudyFactor / ObsNum "
  M1<-rma.mv(g,Vg,random = formula(RandomTermString),
             test="z", data=FPData, method="REML")
  summary(M1)

  WtMatrix<-weights(M1,type="matrix")
  WtRowSum<-apply(WtMatrix,1,sum)
  FPData$Weights<-WtRowSum/sum(WtRowSum)


# put study information in a table for the plot
# %>% rename("PKU Mean"=ExpMean, "PKU SD"=ExpSD, "PKU N"=ExpN, "Control mean" = ControlMean, "Control SD" = ControlSD, "Control N" = ControlN, "Glass delta" = SMD)
  PlotTableRaw <- FPData %>% select(StudyName, ShortTitle, ExpMean, ExpSD, ExpN, ControlMean, ControlSD, ControlN, Weights)
  PTable<-data.frame(
    StudyName=sprintf("%s",PlotTableRaw$StudyName),
    MeasureName=sprintf("%s",PlotTableRaw$ShortTitle),
    ExpMean=sprintf("%6.2f",PlotTableRaw$ExpMean),
    ExpSD=sprintf("%6.2f", PlotTableRaw$ExpSD),
    ExpN=sprintf("%3d",PlotTableRaw$ExpN),
    ControlMean=sprintf("%6.2f",PlotTableRaw$ControlMean),
    ControlSD=sprintf("%6.2f", PlotTableRaw$ControlSD),
    ControlN=sprintf("%3d",PlotTableRaw$ControlN),
    Weight=sprintf("%.1f",PlotTableRaw$Weights*100)
  )

  if(UsePlotRangeDefaults){ # limits to forest plot
    MinES <- -3 
    MaxES <- 2
  }
  
  if(UsePlotPercentDefaults){ # horizontal space percentages allocated to 
    # different parts of the forest plot
    PlotPercentage<- .35  # percentage of space devoted to forest plot (vs text)
    StudyNamePercent<-0.25 # percent of _text_ area for study name
    MeanPercent<-0.085 # width of columns for means - % of total _text_ area
    SDPercent<-0.06 # width of column for SD
    NPercent<-0.04 # width of column for N
    WtPercent<-0.04
  }
  
  LittleSep<-0.002 # separation between columns in a group (% of _text_ area)
  BigSep<-0.004 # separation between columns in different groups (% of _text_ area)
  TextMargin<-0.01 # space between text and plot (% of text area)
  
  ColumnSep<-c(LittleSep,LittleSep,LittleSep,LittleSep,BigSep,LittleSep,LittleSep,LittleSep)
  ColumnWidths<-c(MeanPercent,SDPercent,NPercent,MeanPercent,SDPercent,NPercent,WtPercent)
  MeasureWidth<-(1-TextMargin)-(sum(ColumnSep)+sum(ColumnWidths)+StudyNamePercent)
  if(MeasureWidth < 0){
    c("******WARNING***** space for Measure is too small ************")
  }
  ColumnWidths<-c(MeasureWidth,ColumnWidths)
  ColumnAlign<-c(4,2,2,2,2,2,2,2) # first is left aligned, others centered
  HeaderAlign<-c(1,1,1,1,1,1,1,1) # center headers
  
  ColumnPositionPercentages<-InterleaveSepsAndWidths(ColumnSep,ColumnWidths,ColumnAlign)
  ColumnPositionPercentages<-ColumnPositionPercentages+StudyNamePercent
  
  HeaderPositionPercentages<-InterleaveSepsAndWidths(ColumnSep,ColumnWidths,HeaderAlign)
  HeaderPositionPercentages<-HeaderPositionPercentages+StudyNamePercent
  
  HeaderLine1<-c("Measure","Mean","SD","N","Mean","SD","N","% Wt")
  HeaderLine2<-c("","","PKU","","","Control","")
  AxisTitle<-"PKU worse        Effect size        PKU better"
  
  HeteroString<-sprintf(" Heterogeneity: Q(%2d) = %3.2f, p<%0.2f, I-squared = %0.3f, Tau-squared = %0.3f", M1$k-1, M1$QE, M1$QEp, ISqr, TauSqr)

  MetaModel<-M1
  TextDF<-PTable
  MyCex<-0.94
  
  ForestPlotWithSummary(FunctionTitle,PlotFilename,MetaModel,PlotPercentage,
                        StudyNamePercent,TextDF,ColumnPositionPercentages,
                        ColumnAlign,HeaderPositionPercentages,HeaderLine1,
                        HeaderLine2,MyCex,AxisTitle,HeteroString,MinES,MaxES)
  # return the statistical model that is the basis for the plot
  return(M1)
}


DoOneManualForestPlot<-function(FPData,RandomTermString,FunctionTitle,
                                g, Vg, my_ES, my_ES_se, my_ES_z_val, my_ES_p_val,
                                PlotFilename,UsePlotRangeDefaults,UsePlotPercentDefaults){
  
  # this step does the statistical model for the meta-analysis
  # g is the effect sizes
  # Vg is the variance of the effect sizes
  # random is what describes the relationship between numbers and studies
  # 1 | StudyNum / ObsNum means that ObsNum (different for each measure) is nested
  # within StudyNum.  "1 |" is needed because this is how random intercepts are
  # described when specifying a mixed-effect model.  1 means intercept and what follows
  # the | is what the intercept is for. 1 | StudyNum / ObsNum means a random intercept for
  # ObsNum nested within StudyNum
  FPData<-FPData[order(FPData$StudyName,FPData$ShortTitle),] # make sure that model results and table results (below)
  # are in the same order
  
  HetResult<-metahet(FPData$g,FPData$Vg)
  ISqr<-HetResult$I2
  Q<-HetResult$Q
  TauSqr<-HetResult$tau2.DL
  
  # typical RandomTermString "~ 1 | StudyFactor / ObsNum "
  M1<-rma.mv(g,Vg,random = formula(RandomTermString),
             test="z", data=FPData, method="REML")
  summary(M1)
  
  WtMatrix<-weights(M1,type="matrix")
  WtRowSum<-apply(WtMatrix,1,sum)
  FPData$Weights<-WtRowSum/sum(WtRowSum)
  
  
  # put study information in a table for the plot
  # %>% rename("PKU Mean"=ExpMean, "PKU SD"=ExpSD, "PKU N"=ExpN, "Control mean" = ControlMean, "Control SD" = ControlSD, "Control N" = ControlN, "Glass delta" = SMD)
  PlotTableRaw <- FPData %>% select(StudyName, ShortTitle, ExpMean, ExpSD, ExpN, ControlMean, ControlSD, ControlN, Weights)
  PTable<-data.frame(
    StudyName=sprintf("%s",PlotTableRaw$StudyName),
    MeasureName=sprintf("%s",PlotTableRaw$ShortTitle),
    ExpMean=sprintf("%6.2f",PlotTableRaw$ExpMean),
    ExpSD=sprintf("%6.2f", PlotTableRaw$ExpSD),
    ExpN=sprintf("%3d",PlotTableRaw$ExpN),
    ControlMean=sprintf("%6.2f",PlotTableRaw$ControlMean),
    ControlSD=sprintf("%6.2f", PlotTableRaw$ControlSD),
    ControlN=sprintf("%3d",PlotTableRaw$ControlN),
    Weight=sprintf("%.1f",PlotTableRaw$Weights*100)
  )
  
  if(UsePlotRangeDefaults){ # limits to forest plot
    MinES <- -3 
    MaxES <- 2
  }
  
  if(UsePlotPercentDefaults){ # horizontal space percentages allocated to 
    # different parts of the forest plot
    PlotPercentage<- .35  # percentage of space devoted to forest plot (vs text)
    StudyNamePercent<-0.25 # percent of _text_ area for study name
    MeanPercent<-0.085 # width of columns for means - % of total _text_ area
    SDPercent<-0.06 # width of column for SD
    NPercent<-0.04 # width of column for N
    WtPercent<-0.04
  }
  
  LittleSep<-0.002 # separation between columns in a group (% of _text_ area)
  BigSep<-0.004 # separation between columns in different groups (% of _text_ area)
  TextMargin<-0.01 # space between text and plot (% of text area)
  
  ColumnSep<-c(LittleSep,LittleSep,LittleSep,LittleSep,BigSep,LittleSep,LittleSep,LittleSep)
  ColumnWidths<-c(MeanPercent,SDPercent,NPercent,MeanPercent,SDPercent,NPercent,WtPercent)
  MeasureWidth<-(1-TextMargin)-(sum(ColumnSep)+sum(ColumnWidths)+StudyNamePercent)
  if(MeasureWidth < 0){
    c("******WARNING***** space for Measure is too small ************")
  }
  ColumnWidths<-c(MeasureWidth,ColumnWidths)
  ColumnAlign<-c(4,2,2,2,2,2,2,2) # first is left aligned, others centered
  HeaderAlign<-c(1,1,1,1,1,1,1,1) # center headers
  
  ColumnPositionPercentages<-InterleaveSepsAndWidths(ColumnSep,ColumnWidths,ColumnAlign)
  ColumnPositionPercentages<-ColumnPositionPercentages+StudyNamePercent
  
  HeaderPositionPercentages<-InterleaveSepsAndWidths(ColumnSep,ColumnWidths,HeaderAlign)
  HeaderPositionPercentages<-HeaderPositionPercentages+StudyNamePercent
  
  HeaderLine1<-c("Measure","Mean","SD","N","Mean","SD","N","% Wt")
  HeaderLine2<-c("","","PKU","","","Control","")
  AxisTitle<-"PKU worse        Effect size        PKU better"
  
  HeteroString<-sprintf(" Heterogeneity: Q(%2d) = %3.2f, p<%0.2f, I-squared = %0.3f, Tau-squared = %0.3f", M1$k-1, M1$QE, M1$QEp, ISqr, TauSqr)
  
  TextDF<-PTable
  MyCex<-0.94
  
  ForestPlotWithManualSummary(FunctionTitle, g, Vg, my_ES, my_ES_se, my_ES_z_val, 
                                my_ES_p_val,  PlotFilename, 
                                        PlotPercentage, StudyNamePercent, TextDF, 
                                        ColumnPositionPercentages, ColumnAlign, HeaderPositionPercentages, 
                                        HeaderLine1, HeaderLine2, MyCex, AxisTitle, 
                                        HeteroString, MinES, MaxES)
  # return the statistical model that is the basis for the plot
  return(M1)
}
