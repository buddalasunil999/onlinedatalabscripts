#inputFilePath - is loaded with path to csv input file
#do not change variable names uploaddata, outputdata, allchoices

processInput <- function(inputFilePath){
	uploaddata <- read.csv(inputFilePath)
	outputdata <- toJSON(uploaddata[1:20,])
	allchoices <- names(uploaddata)
	
	return(list("uploaddata"= uploaddata, "outputdata" = outputdata, "allchoices"= allchoices))
}