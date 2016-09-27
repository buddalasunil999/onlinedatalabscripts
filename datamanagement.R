#variables that are loaded listed below 
#outputPath - is where plot will be saved
#choiceType - is contains either 'Continuous' or 'Category'
#choices - is loaded with a data.frame of choices user chosen
#result - is loaded with table results as JSON

dataManagement <- function(choices, choiceType, outputPath){
	noOfChoices <- length(choices)
	svg(outputPath)

	resultData <- c()

	if(choiceType == 'Category') {
	limit <- as.integer(noOfChoices/2) + noOfChoices%%2
	layout(matrix(c(1:(limit*2)), nrow = limit, byrow = TRUE))
		
	for (i in 1:noOfChoices) {
		table_solution_count <- table(choices[,i])
		table_solution_percent=paste(round(table_solution_count/sum(table_solution_count),digit=4)*100,"%")
		barplot(table_solution_count,main="frequency distribution")		
		resultData <- append(resultData, list(rbind(names(table_solution_count), as.character(table_solution_count), table_solution_percent)))
	}
	} else {
	continuousResult <- matrix(,nrow=noOfChoices,ncol=length(as.character(summary(choices[,1]))))
	par(mfcol=c(noOfChoices,3))

	for (i in 1:noOfChoices)
	{
		hist(choices[,i],main=paste("histogram of", names(choices)[i]),na.rm=T)
		continuousResult[i,] <- as.character(summary(choices[,i]))
	}

	resultData <- list(names(summary(choices[,1])), continuousResult)

	for (i in 1:noOfChoices)
	{boxplot(choices[,i],main=paste("boxplot of", names(choices)[i]),na.rm=T)}

	for (i in 1:noOfChoices)
	{qqnorm(choices[,i],main="normal qq plot check normality",na.rm=T)}
	}

	dev.off()
	
	result <- toJSON(resultData)
	
	return(result)
}