#bivariate data analysis

#####################################Bivariate analysis continuous ~ categorical###################################################################
#visualization first-function 6
bi_plot <- function(varname,dependentname,class,testdata, choices, outputPath1, outputPath2,outputPath4,color)
{
	svg(outputPath1)
	group=table(class)
	noclass=length(table(class))
	par(mfrow=c(1,noclass))
	for (i in 1:noclass){
		hist(varname[class==attributes(group[i])$names],main=paste(dependentname,"histogram"),xlab=attributes(group[i])$names,col=color)
	}
	dev.off()
	
	svg(outputPath2)
	boxplot(varname~class,main= paste("boxplot of ", dependentname),data=testdata,col=color)
	dev.off()
	
	svg(outputPath4)
	layout(cbind(1,2), widths=c(7,1))
	xprob=seq(0,1,by=0.05)
	k=length(group)
	contents=names(group)
	for ( i in 1:k){
		q=quantile(varname[class==contents[i]],na.rm=TRUE,probs=seq(0,1,0.05))
		if (i == 1) {
			plot(q,xprob,xlim=c(min(varname,na.rm=TRUE),max(varname,na.rm=TRUE)),type="n",ylab="quantile",xlab=dependentname,main=paste("cumulative probability plot",dependentname))
		}
		lines(q,xprob,lty=i,lwd=2)
    }	
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend('center',contents,lty=1:k,cex=0.6)
	dev.off()
}

#Summary analysis by subgroup (a categorical variable)
bi_mean_class<-function(varname,class) 
{
	meanResult <- c()
	group=attributes(table(class))$dimnames$class
	grouplength=length(group)
	for ( i in (1:grouplength))	{
		meanvar=mean(subset(varname,class==group[i]),na.rm=T)
		meanResult <- append(meanResult, meanvar)
	}
	names(meanResult) <- group	
	return(meanResult)
}

bi_summary_class=function(varname,class)
{
	group=attributes(table(class))$dimnames$class
	grouplength=length(group)
	summaryList <- vector("list", length=grouplength)
	for ( i in (1:grouplength))	{
		summaryvar=summary(subset(varname,class==group[i]),na.rm=T)
		summaryList[[i]] <- as.list(summaryvar)
	}
	
	names(summaryList) <- group
	return(summaryList)
}

#Hypothesis test -parametric test-t test two samples
bi_ttest=function(varname,class,testdata){
	dataList <- vector("list", length=6)
	group=table(class)
	difference=mean(varname[class==attributes(group[1])$names])-mean(varname[class==attributes(group[2])$names])
	dataList[[1]] <- list("mean difference" = difference)
	dataList[[2]] <- as.list(t.test(varname~class,data=testdata)$statistic)
	dataList[[3]] <- as.list(t.test(varname~class,data=testdata)$parameter)
	pvalue <- as.list(t.test(varname~class,data=testdata)$p.value)
	dataList[[4]] <- c("p value of t test" = pvalue)
	conf_int <- t.test(varname~class,data=testdata)$conf.int
	dataList[[5]] <- list("confidence interval of difference" = conf_int)
	dataList[[6]] <- as.list(t.test(varname~class,data=testdata)$estimate)       #output this -need to sort out the parameters of t ,df, p-value,95 percent confidence interval and sample estimates of mean
	
	return(dataList)
}

#Hypothesis test - parametric test - paired sample t test
bi_ttest_paired=function(varname,class,testdata)
{
summary <- capture.output(t.test(varname~class,paired=T,data=testdata))
summary <- c(summary, "of note: 95% confidennce interval is for the estimate of mean difference")
return(summary)
	# output nees t, df, p-value, 95 percent confidence interval, sample estimates
}

#Hypothesis
#Hypothesis test - non parametric test- Kolmogorov-Smirnov test. 
bi_kstest=function(varname,class, testdata)
{
	with(testdata,{
	group=table(class);
	return(capture.output(ks.test(varname[class==attributes(group[2])$names],varname[class==attributes(group[1])$names], alternative = "l")))})
}

#Analysis of variance 		
bi_anova=function(varname,class,testdata,outputPath2,color)
{
	dataList <- vector("list", length=3)
	
	fit=glm(varname~factor(class),family=gaussian,data=testdata)
	coefficientsVal=fit$coefficients;   		#print this as one cell in the table
	aic=summary(fit)$aic						#print this as one cell in the table 	
	devianceval=summary(fit)$deviance				#print this as one cell in the table

	coefficientsFootNote <- c("Of Note: the presented coefficients are differences between means of each category versus. reference category.", "*Reference category is the lowest level of the independent variable", "for example: the coefficient is the mean difference in BMI between severity 2 vs. 1, 3 vs. 1 and 4 vs. 1 respectively.")
	
	dataList[[1]] <- list(as.list(coefficientsVal), "footnote" = coefficientsFootNote)
	dataList[[2]] <- as.list(aic)
	dataList[[3]] <- as.list(devianceval)
	names(dataList) <- c("coefficients of Analysis of Variance","AIC (Akaike Information Criterion)","residual deviance")
	
	resultData <- list(toJSON(dataList), capture.output(summary(fit)))	#print this as one large cell in the table	
	
	svg(outputPath2)
	qqnorm(fit$residuals,main="qq normal plot for diagnosis of the model",col=color)
	dev.off()

	return(resultData)
}

bivariate_cont_cat <- function(choices, uploaddata, outputPath1, outputPath2,outputPath3,outputPath4,color){
    dependent <- uploaddata[[choices[1]]]
	independent <- uploaddata[[choices[2]]]
	valuesList <- vector("list", length=5)
	
	noclass<-length(table(independent))
	
	bi_plot(dependent, choices[1], independent,uploaddata, choices, outputPath1, outputPath2, outputPath4, color)
	meanClass <- bi_mean_class(varname=dependent,class=independent)
	valuesList[[1]] <- toJSON(as.list(meanClass))
	summaryList <- bi_summary_class(varname=dependent,class=independent)
	valuesList[[2]] <- toJSON(summaryList)
	if(noclass == 2){
		ttest <- bi_ttest(dependent,independent,testdata=uploaddata)
		valuesList[[3]] <- toJSON(ttest)
	}
	kstest <- bi_kstest(dependent,independent,uploaddata)  				#need to output all informations p value
	valuesList[[4]] <- kstest
	if(noclass > 2){
		anovaResult <- bi_anova(dependent,independent,uploaddata,outputPath3,color)
		valuesList[[5]] <- anovaResult
	}
	
	names(valuesList) <- c("Mean", "Summary", "ttest", "kstest", "anova")
	
	return(valuesList)
}

bivariate_cont_cat_paired <- function(choices, uploaddata, outputPath1,outputPath2,outputPath3, color){
	dependent <- uploaddata[[choices[1]]]
	independent <- uploaddata[[choices[2]]]
	
	valuesList <- vector("list", length=4)
	
	bi_plot(dependent,choices[1],independent,uploaddata, choices, outputPath1,outputPath2,outputPath3, color)
	meanClass <- bi_mean_class(varname=dependent,class=independent)
	summaryList <- bi_summary_class(varname=dependent,class=independent)	
	ttestPaired <- bi_ttest_paired(dependent,independent,uploaddata)
	
	valuesList[[1]] <- toJSON(as.list(meanClass))
	valuesList[[2]] <- toJSON(summaryList)
	valuesList[[3]] <- ttestPaired
	names(valuesList) <- c("Mean", "Summary", "ttestPaired")
	
	return(valuesList)
}