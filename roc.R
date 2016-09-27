best_threshold <- function(x, digits=max(3, getOption("digits") - 3), ...) {
  label <- paste(attr(x, "conf.level")*100, "% CI", sep="")
  label <- paste(label, paste(" (", attr(x, "boot.n"), " ", ifelse(attr(x, "boot.stratified"), "stratified", "non-stratified"), " bootstrap replicates):", sep=""), sep="")
  signif.sp <- signif(x$sp, digits=digits)
  signif.se <- signif(x$se, digits=digits)
  content <- data.frame(thresholds=attr(x, "thresholds"), sp.low=signif.sp[,1], sp.median=signif.sp[,2], sp.high=signif.sp[,3], se.low=signif.se[,1], se.median=signif.se[,2], se.high=signif.se[,3]);
  return(setNames(list(content),label))
} 

bi_ROC=function(outcome,test,color,uploaddata,outputPath)
{
valuesList <- vector("list", length=2)
  
svg(outputPath)
rocobj <- plot.roc(outcome, test,  main="ROC curve", percent=TRUE,  ci=TRUE, reuse.auc=T,legacy.axes=T,print.auc=TRUE,print.auc.x=50, print.auc.y=50) # print the AUC  

ciobj <- ci.se(rocobj, specificities=seq(0, 100, 5)) 				#over a select set of specificities
plot(ciobj, type="shape", col=color)             
#plot as a green shape  
title <- "the best threshold of optimal sensitivity and specificity" 	#print this as title
tableValue <- best_threshold(ci(rocobj, of="thresholds", thresholds="best")) 			#print the best threshold in first table
valuesList[[1]] <- setNames(list(tableValue),title)
rocobj=roc(outcome ~ test, ci=T,data=uploaddata)
summarythreshold=data.frame(rocobj$thresholds,rocobj$sensitivities,rocobj$specificities)
valuesList[[2]] <- list(as.list(summarythreshold))									#print this as the second table
dev.off();
return(valuesList)
}
bi_ROC2=function(outcome,testmatrix,color,uploaddata,outputPath,show.confid=F)
{
ntestno <- length(testmatrix);
valuesList <- vector("list", length=2*ntestno)
  
svg(outputPath)
rocobj <- plot.roc(outcome, testmatrix[,1],main="ROC curve",percent=TRUE,ci=TRUE,reuse.auc=T,legacy.axes=T,print.auc=F) # print the AUC  

legend(50,50,lty=1,names(testmatrix)[1],bty="n")
if (show.confid) 
	{ciobj <- ci.se(rocobj, specificities=seq(0, 100, 5)) 				#over a select set of specificities  
	plot(ciobj,type="shape",col="#0000ff11",lty=1) 
	}

#print the best threshold in table outcome1  
label <- names(testmatrix)[1]								#print this as label for test 1
tableValue <- best_threshold(ci(rocobj, of="thresholds", thresholds="best"))				#print this as set 1 -table 1 for test 1.
valuesList[[1]] <- setNames(list(tableValue),label)
for (i in 2:ntestno)
	{
rocobj2=lines.roc(outcome,testmatrix[,i],percent=TRUE,col=color,ci=TRUE,lty=i) 
legend(50,50-i*2,lty=i,names(testmatrix)[i],bty="n",col=color)
if (show.confid)
	{ciobj2 <- ci.se(rocobj2, specificities=seq(0, 100, 5))
	plot(ciobj2,type="shape",lty=i,col="#0000ff11")
      }
label <- names(testmatrix)[i];								#print this as label for test i 
tableValue <- best_threshold(ci(rocobj2, of="thresholds", thresholds="best")) 			#print this as set 1- table i for test i (i > 2)
valuesList[[i]] <- setNames(list(tableValue),label)
	}

for(i in 1:ntestno)
	{
	rocobj=roc(outcome ~ testmatrix[,i], ci=T,data=uploaddata)
	label <- paste(names(testmatrix)[i],"'s AUC",sep="")
	aucval <- capture.output(rocobj$auc);
	cival <- capture.output(rocobj$ci);

	summarythreshold=data.frame(rocobj$thresholds,rocobj$sensitivities,rocobj$specificities)
	title <- paste(names(testmatrix)[i],"coordinates with each pair of sen and spe")								#print this as label for each test 
	tableVal <- setNames(list(as.list(summarythreshold)), title)									#print this as the set 2 of table for each test
	valuesList[[ntestno+i]] <- list(label,aucval,cival,tableVal)
	}
	
	dev.off();
	return(valuesList);
}

rocAnalysis <- function(outcome, choices, color, uploaddata, showConf, outputPath){	
    resultList <- c()
	if(length(choices) == 1){	
		resultList <- bi_ROC(uploaddata[[outcome]],uploaddata[[choices[1]]],color,uploaddata,outputPath)
	}
	else{
		testData <- data.frame(uploaddata[[choices[1]]])
		for(i in 2:length(choices)){
		 testData <- cbind(testData, uploaddata[[choices[i]]]);
		}
		names(testData) <- choices;
		resultList <- bi_ROC2(uploaddata[[outcome]],testData,color,uploaddata,outputPath,showConf)
	}
	return(toJSON(resultList));
}