#Bivariance analysis categorical ~ continuous		#when cont vs. con, cat has to have only two subclass
#function 1: logistic regression 

bi_cat_con1=function(choices, varname1, varname2, testdata)
{
valuesList <- vector("list", length=3)
#logistic regression for dependant variable with 2 subclass
fit=glm(varname1~varname2,family=binomial)		
labelvar2=names(table(varname1))			
oddsRatioLabel <- paste("odds ratio of having the upper level of ", choices[[1]], ": ", labelvar2[2])						#print this with lable "odds ratio" -with the odds ratio numner in a cell					
valuesList[[1]] <- setNames(list(exp(summary(fit)$coefficients[2,1])), oddsRatioLabel)

#print this with label "odds ratio's 95% confidence interval" in a cell
ci=c(exp(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2]),exp(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2]))
valuesList[[2]] <- list("95% confidence interval of odds ratio"=ci)
fitSummary<-capture.output(fit)
valuesList[[3]] <- replace(fitSummary, fitSummary=="", "\n")					#print list of summary
return(valuesList)
}

bi_cat_con2=function(choices, varname1, varname2, testdata)
{
valuesList <- vector("list", length=4)
fit2=multinom(varname1~varname2)		
nclass=length(table(varname1))

labelvar2=names(table(varname1))			
oddsRatioLabel <- paste("odds ratio of having the upper level of ", choices[[1]], ": ",labelvar2[2:nclass])
oddsratio=(exp(summary(fit2)$coefficients[,2]))					#print this with lable "odds ratio" -with the odds ratio numner in a cell	
names(oddsratio) <-oddsRatioLabel
valuesList[[1]] <- list("odds ratio"=as.list(oddsratio))

#print this with label "odds ratio's 95% confidence interval: lower bound" in a cell
ci_low=(exp(summary(fit2)$coefficients[,2]-1.96*summary(fit2)$standard.error[,2]))
valuesList[[2]] <- list("Lower boundary of 95% confidence interval of odds ratio"=as.list(ci_low))

#print this with label "odds ratio's 95% confidence interval: upper bound" in a cell
ci_upper=(exp(summary(fit2)$coefficients[,2]+1.96*summary(fit2)$standard.error[,2]))
valuesList[[3]] <- list("Upper boundary of 95% confidence interval of odds ratio"=as.list(ci_upper))

fitSummary <- capture.output(summary(fit2))									#print summary
valuesList[[4]] <- replace(fitSummary, fitSummary=="", "\n")
return(valuesList)
}

bivariate_cat_cont <- function(choices, dependent, independent, uploaddata){
	noclass<-length(table(dependent))
	if(noclass == 2){
		return(toJSON(bi_cat_con1(choices, dependent,independent,uploaddata)))
	}
	else{
		return(toJSON(bi_cat_con2(choices, dependent,independent,uploaddata)))
	}
}
