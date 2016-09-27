########################################################Bivariate analysis continuous ~ continuous############################################
#There are X functions for bivariate analysis with continuous response~continuous
#function 1: correlation and scatter plots
#function 2: linear regression
#function 3: smooth regression (loess regression -quantile regression)

#need to install this package for bi_con_con

bi_con_con=function(varname1, varname2, testdata, choices, outputPath1, outputPath2, color)
{
	#scatter plot
	svg(outputPath1)				
	plot(varname2,varname1,xlab=choices[2],ylab=choices[1],main="scatter plot",col=color,pch=19,cex=1.5);
	dev.off()
	
	valuesList <- vector("list", length=8)
	
	#correlations
	cor_test=cor.test(varname1,varname2,na.rm=T,method="pearson")
	valuesList[[1]] <- list("pearson correlation estimate" = as.list(cor_test$estimate))
	valuesList[[2]] <- list("95% confidence interval for pearson correlation coefficients" = cor_test$conf.int)

	cor_test2=cor.test(varname1,varname2,na.rm=T,method="spearman")
	valuesList[[3]] <- list("spearman correlation estimate" = as.list(cor_test2$estimate))
	valuesList[[4]] <- list("95% confidence interval for spearman coefficients" = cor_test2$conf.int)

	#linear regressions
	fit=summary(lm(paste(choices[1], "~", choices[2]),testdata))
	valuesList[[5]] <- list("coefficients" = as.data.frame(fit$coefficients))
	valuesList[[6]] <- list("r squared" = fit$r.squared)
	
	fitValue <- capture.output(fit)
	valuesList[[7]] <- replace(fitValue, fitValue=="", "\n")

	#quantile linear regression
	#ref: http://data.library.virginia.edu/getting-started-with-quantile-regression/
	qs=0.5
	fitq=summary(rq(paste(choices[1], "~", choices[2]), data=testdata, tau=qs))
	valuesList[[8]] <- list("quantile regression results for median" = as.data.frame(fitq$coefficients))

	svg(outputPath2)
	qqnorm(fitq$residuals,col=color)											#check the model fit
	dev.off()
	
	return(valuesList)
}

bivariate_cont_cont <- function(choices, uploaddata, outputPath1, outputPath2, color){
	dependent <- uploaddata[[choices[1]]]
	independent <- uploaddata[[choices[2]]]
	resultList <- bi_con_con(dependent,independent,uploaddata, choices, outputPath1, outputPath2, color)
	return(toJSON(resultList))
}