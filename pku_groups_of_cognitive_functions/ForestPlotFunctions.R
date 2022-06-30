MyForestPlot<-function(MeasureTitle, MetaModel, TextMin, MinES, MaxES, 
                       TextDF, TextPos, TextAlign, HeaderPos, HeaderLine1, HeaderLine2,
                       SummaryString, HeteroString, 
                       MyCex,AxisTitle,MyYLim){
  forest(MetaModel,
         annotate=FALSE,
         showweights=FALSE,
         header=FALSE,
         xlim=c(TextMin,MaxES),
         alim=c(MinES,MaxES),
         at=seq(MinES,MaxES,1),
         slab=TextDF$StudyName,
         ilab = TextDF[,seq(2,length(TextDF[1,]))],
         ilab.xpos = TextPos,
         ilab.pos = TextAlign,
         mlab = SummaryString,
         cex=MyCex,
         efac=c(1,3),
         xlab=AxisTitle,
         ylim=MyYLim,
         top=4
  )
  
  # add the headers for the columns
  text(TextMin,length(TextDF[,1])+2,"Study",cex=MyCex,adj=0)
  text(TextMin,length(TextDF[,1])+4,MeasureTitle,cex=MyCex,adj=0,font=2)
  text(HeaderPos,
       length(TextDF[,1])+2,
       HeaderLine1,
       cex=MyCex)
  text(HeaderPos,length(TextDF[,1])+3,HeaderLine2,cex=MyCex,font=2)
  text(TextMin+0.16,-2,HeteroString,cex=MyCex,adj=0)
}

MyManualForestPlot<-function(MeasureTitle, g, Vg, my_ES, my_ES_se, TextMin, MinES, MaxES, 
                       TextDF, TextPos, TextAlign, HeaderPos, HeaderLine1, HeaderLine2,
                       SummaryString, HeteroString, 
                       MyCex,AxisTitle,MyYLim){ 
  # ES and ES_se and SummaryString are passed manually for summary line and summary polygon
  
    forest(g,Vg,
         annotate=FALSE,
         showweights=FALSE,
         header=FALSE,
         xlim=c(TextMin,MaxES),
         alim=c(MinES,MaxES),
         at=seq(MinES,MaxES,1),
         slab=TextDF$StudyName,
         ilab = TextDF[,seq(2,length(TextDF[1,]))],
         ilab.xpos = TextPos,
         ilab.pos = TextAlign,
#         mlab = SummaryString,
         cex=MyCex,
         efac=c(1,3),
         xlab=AxisTitle,
         ylim=MyYLim,
         top=4
  )

  addpoly(my_ES, sei=my_ES_se, mlab = SummaryString, cex=MyCex, annotate=FALSE)
  
  # add the headers for the columns
  text(TextMin,length(TextDF[,1])+2,"Study",cex=MyCex,adj=0)
  text(TextMin,length(TextDF[,1])+4,MeasureTitle,cex=MyCex,adj=0,font=2)
  text(HeaderPos,
       length(TextDF[,1])+2,
       HeaderLine1,
       cex=MyCex)
  text(HeaderPos,length(TextDF[,1])+3,HeaderLine2,cex=MyCex,font=2)
  text(TextMin+0.16,-2,HeteroString,cex=MyCex,adj=0)
}


ForestPlotWithSummary<-function(MeasureTitle, PlotFilename, MetaModel, 
                                PlotPercentage, StudyNamePercent, TextDF, 
                                ColumnPositionPercentages, ColumnAlign, HeaderPositionPercentages, 
                                HeaderLine1, HeaderLine2, MyCex, AxisTitle, 
                                HeteroString, MinES=NULL, MaxES=NULL){
  # make a forest plot for the model (M1) that we calculated above
  # slab are the labels for the studies
  if(is.null(MinES)){
    MinES<-round(min(MetaModel$yi))-1 # automatically set limits
  }
  if(is.null(MaxES)){
    MaxES<-round(max(MetaModel$yi))+1
  }
  PlotRange<-MaxES - MinES # size of ES area in plot coordinates
  TextRange<-PlotRange/PlotPercentage
  FullRange<-TextRange+PlotRange
  TextMin<-MinES-TextRange
  TextMax<-MinES
  
  TextPos<-(ColumnPositionPercentages*TextRange)+TextMin
  HeaderPos<-(HeaderPositionPercentages*TextRange)+TextMin
  
  # get summary information from the model
  PValue<-MetaModel$pval
  ZValue<-MetaModel$zval
  CILower<-MetaModel$ci.lb
  CIUpper<-MetaModel$ci.ub
  ESEstimate<-MetaModel$beta
  # create the summary string for the bottom of the plot
  SummaryString<-sprintf("Pooled effect size: %1.3f ci = (%1.3f, %1.3f) z = %0.2f, p = %0.4f", 
                         MetaModel$beta, MetaModel$ci.lb, MetaModel$ci.ub, ZValue, PValue)
  
  # do the plotting
  MyForestPlot(MeasureTitle, MetaModel, TextMin, MinES, MaxES, 
               TextDF, TextPos, TextAlign=ColumnAlign, HeaderPos,HeaderLine1,HeaderLine2,
               SummaryString, HeteroString, 
               MyCex,AxisTitle)
  LineHeight<-6
  ConstantHeight<-80
  MyYlim<-c(-3,length(TextDF[,1])+4)
  PlotHeight<-ConstantHeight+(LineHeight*length(TextDF[,1]))
  tiff(PlotFilename,width=320, height=PlotHeight, units="mm",res=300)
  MyForestPlot(MeasureTitle,MetaModel, TextMin, MinES, MaxES, 
               TextDF, TextPos, TextAlign=ColumnAlign, HeaderPos,HeaderLine1,HeaderLine2,
               SummaryString, HeteroString,
               MyCex,AxisTitle,MyYlim)
  dev.off()
}

# R> 
# R> res <- rma(yi, vi, mods = cbind(ablat), data = dat)
# R> preds <- predict(res, newmods = c(10, 30, 50))
# R> addpoly(preds$pred, sei = preds$se, atransf = exp,
#            + mlab = c("10 Degrees", "30 Degrees", "50 Degrees"))
# R> text(-9, 15, "Author(s) and Year", pos = 4, font = 2)
# R> text(7, 15, "Relative Risk [95% CI]", pos = 2, font = 2)
# R> abline(h = 0)

ForestPlotWithManualSummary<-function(MeasureTitle, g, Vg, my_ES, my_ES_se, 
                                      my_ES_z_val, my_ES_p_val, PlotFilename, 
                                PlotPercentage, StudyNamePercent, TextDF, 
                                ColumnPositionPercentages, ColumnAlign, HeaderPositionPercentages, 
                                HeaderLine1, HeaderLine2, MyCex, AxisTitle, 
                                HeteroString, MinES=NULL, MaxES=NULL){
  
  
  # make a forest plot for the model (M1) that we calculated above
  # slab are the labels for the studies
  if(is.null(MinES)){
    MinES<-round(min(g))-1 # automatically set limits
  }
  if(is.null(MaxES)){
    MaxES<-round(max(g))+1
  }
  PlotRange<-MaxES - MinES # size of ES area in plot coordinates
  TextRange<-PlotRange/PlotPercentage
  FullRange<-TextRange+PlotRange
  TextMin<-MinES-TextRange
  TextMax<-MinES
  
  TextPos<-(ColumnPositionPercentages*TextRange)+TextMin
  HeaderPos<-(HeaderPositionPercentages*TextRange)+TextMin
  
  # create the summary string for the bottom of the plot
  
  my_ES_ci_lb <- my_ES - (1.96*my_ES_se)
  my_ES_ci_ub <- my_ES + (1.96*my_ES_se)
  SummaryString<-sprintf("Pooled effect size: %1.3f ci = (%1.3f, %1.3f) z = %0.2f, p = %0.4f", 
                         my_ES, my_ES_ci_lb, my_ES_ci_ub, my_ES_z_val, my_ES_p_val)
  
  MyYLim<-c(-1,length(TextDF[,1])+4) # create space for polygon at the bottom (and consider header at the top)
  
  MyManualForestPlot(MeasureTitle, g, Vg, my_ES, my_ES_se, TextMin, MinES, MaxES, 
               TextDF, TextPos, TextAlign=ColumnAlign, HeaderPos, HeaderLine1, HeaderLine2,
               SummaryString, HeteroString, 
               MyCex, AxisTitle, MyYLim)
  LineHeight<-6
  ConstantHeight<-80

  PlotHeight<-ConstantHeight+(LineHeight*length(TextDF[,1]))
  tiff(PlotFilename,width=320, height=PlotHeight, units="mm",res=300)
  MyManualForestPlot(MeasureTitle, g, Vg, my_ES, my_ES_se, TextMin, MinES, MaxES, 
                     TextDF, TextPos, TextAlign=ColumnAlign, HeaderPos, HeaderLine1, HeaderLine2,
                     SummaryString, HeteroString, 
                     MyCex, AxisTitle,MyYLim)
  dev.off()
}

InterleaveSepsAndWidths<-function(Seps,Widths,Align){
  # take two sets and interleave them, adding values as you go
  # (e.g. takes separator width and column width arrays and 
  # returns centered positions)
  if(length(Seps) != length(Widths)){
    stop("****WARNING***** Separator and Width arrays must be the same length")
    return(NULL)
  }
  InterleavedSet<-numeric(length=length(Seps))
  if(Align[1] == 1){
    InterleavedSet[1]<-Seps[1]+Widths[1]/2
    NewStart<-InterleavedSet[1]+Widths[1]/2
  }else if(Align[1] == 2){
    InterleavedSet[1]<-Widths[1]
    NewStart<-InterleavedSet[1]
  }else if(Align[1] == 4){
    InterleavedSet[1]<-0
    NewStart<-Widths[1]
  }else{
    stop("******* WARNING ****** unknown Alignment value.  Must be 1 (c) , 2 (r) or 4 (l)")
  }
  for(i in seq(2,length(Seps))){
    if(Align[i] == 1){
      InterleavedSet[i]<-NewStart+Seps[i]+Widths[i]/2
      NewStart<-InterleavedSet[i]+Widths[i]/2
    }else if(Align[i] == 2){
      InterleavedSet[i]<-NewStart+Seps[i]+Widths[i]
      NewStart<-InterleavedSet[i]
    }else if(Align[i] == 4){
      InterleavedSet[i]<-NewStart+Seps[i]
      NewStart<-InterleavedSet[i]+Widths[i]
    }else{
      stop("******* WARNING ****** unknown Alignment value.  Must be 1 (c) , 2 (r) or 4 (l)")
    }
  }
  return(InterleavedSet)
}