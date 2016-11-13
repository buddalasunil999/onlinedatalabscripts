##############################################################Bivariate analysis categorical ~ categorical################################################################################## 
#There are x functions for bivariate analysis with categorical response ~ categorical independants/predictors
#function 1: chi square or fisher exact test 
#function 2: logistic regression 

bi_cat_cat1=function(choices, varname1, varname2, testdata)
{
	valuesList <- vector("list", length=8)
	fit=table(varname1,varname2)
	fitmatrix <- as.data.frame.matrix(fit)
	valuesList[[1]] <- list("Two-way table counts" = fitmatrix, "rows" = rownames(fitmatrix))
	#row percent
	i=dim(fit)[1]	#no of row
	#column percent
	j=dim(fit)[2]	#no of colume


rowper=matrix(nrow=i,ncol=j)

for (i in 1:i) 
{
	for ( j in 1:j)
{rowper[i,j]=fit[i,j]/rowSums(fit)[i]
}
}
rownames(rowper)<- names(table(varname1))
colnames(rowper)<- names(table(varname2))
rowpermatrix <- as.data.frame(round(rowper,4))
valuesList[[2]] <- list("row proportion" = rowpermatrix, "rows" = rownames(rowpermatrix))

#chi square test
fit2=chisq.test(varname1,varname2)
fit2val<-capture.output(fit2)											#print this with label "chi square test"-with warning message as one table cell
valuesList[[3]] <- replace(fit2val, fit2val=="", "\n")
#fisher exact test
fit3=fisher.test(varname1,varname2)
fit3val<-capture.output(fit3)										#print this with label "fisher exact test"- with warning message as one table cell
valuesList[[4]] <- replace(fit3val, fit3val=="", "\n")

#logistic regression for dependant variable with 2 subclass 
nclass=length(table(varname2))
fit4=glm(varname1~factor(varname2),family=binomial)		
labelvar2=names(table(varname2))			
footnote <- c("Of Note: the presented odds ratios and their confidence intervals are ratios in odds between each category versus. the reference category.", "*Reference category is the lowest level of the independent variable.")
oddsRatioLabel <- paste("odds ratio of each category vs. the lowest category of ", choices[[2]], ": ",labelvar2[1])
#print this with lable and the odds ratio number in a cell					
valuesList[[5]] <- setNames(list(as.list(exp(summary(fit4)$coefficients[2:nclass,1])), footnote),c(oddsRatioLabel,"footnote"))			#odds ratio for every subclass i (i > 1) vs. subclass=1, no intercept

#print this with label "odds ratio's 95% confidence interval: lower bound" in a cell
ci_low= as.list(exp(summary(fit4)$coefficients[2:nclass,1]-1.96*summary(fit4)$coefficients[2:nclass,2]))
valuesList[[6]] <- list("Lower boundary of 95% confidence interval of odds ratio" = ci_low, "footnote"=footnote)

#print this with label "odds ratio's 95% confidence interval: upper bound" in a cell
ci_upper=as.list(exp(summary(fit4)$coefficients[2:nclass,1]+1.96*summary(fit4)$coefficients[2:nclass,2]))
valuesList[[7]] <- list("Upper boundary of 95% confidence interval of odds ratio" = ci_upper, "footnote"=footnote)

fit4val <- capture.output(summary(fit4))
valuesList[[8]] <- replace(fit4val, fit4val=="", "\n")						#print list of summary					    

return(toJSON(valuesList))
}

bi_cat_cat2=function(choices, varname1, varname2, testdata)
{
	valuesList <- vector("list", length=8)
	fit=table(varname1,varname2)						#print this with label "Two-way table counts with varname1 with varname2"
	fitmatrix <- as.data.frame.matrix(fit)
	valuesList[[1]] <- list("Two-way table counts" = fitmatrix, "rows"=rownames(fitmatrix))

	#row percent
	i=dim(fit)[1]	#no of row
	#column percent
	j=dim(fit)[2]	#no of colume


	rowper=matrix(nrow=i,ncol=j)

	for (i in 1:i) 
	{
		for ( j in 1:j)
		{rowper[i,j]=fit[i,j]/rowSums(fit)[i]}
	}
	rownames(rowper)<- names(table(varname1))
	colnames(rowper)<- names(table(varname2))
	rowpermatrix <- as.data.frame.matrix(round(rowper,4))
	valuesList[[2]] <- list("row proportion" = rowpermatrix, "rows"=rownames(rowpermatrix))

	#chi square test
	fit2=chisq.test(varname1,varname2)
	fit2val<-capture.output(fit2)
	valuesList[[3]] <- replace(fit2val, fit2val=="", "\n")			#print this with label "chi square test"-with warning message as one table cell
	#fisher exact test
	fit3=fisher.test(varname1,varname2)
	fit3val<-capture.output(fit3)
	valuesList[[4]] <- replace(fit3val, fit3val=="", "\n")   #print this with label "fisher exact test"- with warning message as one table cell  
	
footnote <- c("Of Note: the presented odds ratios and their confidence interval are ratios in odds between each category versus. the reference category.", "*Reference category is the lowest level of the independent variable.")
	#multinormial logistic regression for dependant variable with more than 2 subclass
	nclass=length(table(varname2))
	fit4=multinom(varname1~factor(varname2))		
	labelvar2=names(table(varname2))			
	oddsLabel <- paste("odds ratio of each category vs the lowest category of ", choices[[2]], "  ",labelvar2[1])
	oddsratio=as.list(exp(summary(fit4)$coefficients[,2:nclass]))									#print this with lable "odds ratio" -with the odds ratio numner in a cell					
	valuesList[[5]] <- setNames(list(oddsratio, footnote),c(oddsLabel,"footnote"))

	#print this with label "odds ratio's 95% confidence interval: lower bound" in a cell
	ci_low=as.list(exp(summary(fit4)$coefficients[,2:nclass]-1.96*summary(fit4)$standard.error[,2:nclass]))
	valuesList[[6]] <- list("Lower boundary of 95% confidence interval of odds ratio" = ci_low, "footnote"=footnote)

	#print this with label "odds ratio's 95% confidence interval: upper bound" in a cell
	ci_upper=as.list(exp(summary(fit4)$coefficients[,2:nclass]+1.96*summary(fit4)$standard.error[,2:nclass]))
	valuesList[[7]] <- list("Upper boundary of 95% confidence interval of odds ratio" = ci_upper, "footnote"=footnote)

	#print list of summary	
	fit4val <- capture.output(summary(fit4))
	valuesList[[8]] <- replace(fit4val, fit4val=="", "\n")

	return(toJSON(valuesList))
}

bivariate_cat_cat <- function(choices, dependent, independent, uploaddata){
	noclass<-length(table(dependent))
	if(noclass == 2){
		return(bi_cat_cat1(choices, dependent,independent,uploaddata))
	}
	else{
		return(bi_cat_cat2(choices, dependent,independent,uploaddata))
	}
}