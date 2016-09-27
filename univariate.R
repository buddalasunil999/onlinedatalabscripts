#Univariate data analysis

#variables that are loaded listed below
#choices - is loaded with data.frame of the variables user has chosen
uni_mean = function(varname) 
{
	meanvar <- mean(varname,na.rm=T)
	return(meanvar)
}
uni_std=function(varname)
{
	std_var=sqrt(var(varname,na.rm=T))
	return(std_var)
}
uni_qt=function (varname) 
{
	return(quantile(varname,probs = seq(0, 1, 0.25), na.rm = TRUE))
}

uniVariate <- function(choices){
	noOfChoices <- length(choices)

	valuesList <- vector("list", length=noOfChoices)

	for (i in 1:noOfChoices)
	{
		meanValue <- uni_mean(choices[,i])	#need to output this mean

		standardDevValue <- uni_std(choices[,i])		#need to output this standard deviation

		quantileValue <- uni_qt(choices[,i])			#need to output this result
		
		valuesList[[i]] <- list("Mean"=meanValue, "Standard Deviation"=standardDevValue, "Quartile"=as.list(quantileValue))
	}

	names(valuesList) <- names(choices)

	output <- toJSON(valuesList)
	
	return(output)
}