#The following are three functions for survival analysis 
#The scond function including cox regression to provide hazard ratio and p value for a catogerical risk factor
#The third function including a risk factor as continuous variable
bi_kp=function(event,followup_time,uploaddata,outputPath,color)
{
  valuesList <- vector("list", length=2)

  survobj=Surv(time=followup_time,event=event) 
  survfor=survfit(survobj~1,data=uploaddata)
  
  printVal <- capture.output(print(survfor))		#print first part of table
  valuesList[[1]] <- replace(printVal, printVal=="", "\n")
  summaryVal <- capture.output(print(summary(survfor)))				#print second part of table
  valuesList[[2]] <- replace(summaryVal, summaryVal=="", "\n")
  svg(outputPath)
  plot(survfor,main="Kaplan Meier Plot",xlab="follow-up time",ylab="event free probability with its 95% CI", col=color)
  dev.off();
  
  return(valuesList);
}

bi_kp2=function(event,followup_time,riskfactor,uploaddata,outputPath,color)
{
  valuesList <- vector("list", length=3)

  ncat=length(table(riskfactor))                #number of category of risk factor
  contents=names(table(riskfactor))
  time_3rdquant=quantile(followup_time,probs=seq(0,1,0.10))[10]
  survobj=Surv(time=followup_time,event=event) 
  survfor=survfit(survobj~riskfactor,data=uploaddata)
  printVal <- capture.output(print(survfor))		#print first part of table
  valuesList[[1]] <- replace(printVal, printVal=="", "\n")
  summaryVal <- capture.output(print(summary(survfor)))				#print second part of table
  valuesList[[2]] <- replace(summaryVal, summaryVal=="", "\n")
  fit<-coxph(Surv(time=followup_time,event=event)~riskfactor,data=uploaddata)
  fitVal <- capture.output(print(fit))
  valuesList[[3]] <- replace(fitVal, fitVal=="", "\n") #print third part of table
  
  svg(outputPath)
  layout(cbind(1,2), widths=c(7,1))
  plot(survfor,lty=1:ncat,main="Kaplan Meier Plot",xlab="follow-up time",ylab="event free probability",col=color) 
  par(mar=c(0, 0, 0, 0))
  plot.new()
  legend('center',contents,lty=1:ncat)
  dev.off();
  
  return(valuesList);
}

bi_kp3=function(event,followup_time,riskcov,uploaddata)
{
	valuesList <- vector("list", length=1)
  fit<-coxph(Surv(time=followup_time,event=event)~riskcov,data=uploaddata)
  fitVal <- capture.output(print(fit))
  valuesList[[1]] <- replace(fitVal, fitVal=="", "\n")	#this will be the only output for a continuous risk factor
  
  return(valuesList);
}

survivalAnalysis = function(choices, type, uploaddata, outputPath,color){	
    resultList <- c()
	if(type== ''){	
		resultList <- bi_kp(uploaddata[[choices[1]]],uploaddata[[choices[2]]],uploaddata,outputPath,color)
	}
	else if(type == 'Continuous'){
		resultList <- bi_kp3(uploaddata[[choices[1]]],uploaddata[[choices[2]]],uploaddata[[choices[3]]],uploaddata)
	}
	else if(type == 'Category'){
		resultList <- bi_kp2(uploaddata[[choices[1]]],uploaddata[[choices[2]]],uploaddata[[choices[3]]],uploaddata,outputPath,color)
	}
	
	return(toJSON(resultList));
}