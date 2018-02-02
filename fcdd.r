
weight <- function(euclidean.dist, sigma) {
	return (exp(-euclidean.dist^2 / (2*sigma^2)))
}

dwnn <- function(knowledge.base, query, sigma=0.5) {

	classId = ncol(knowledge.base)

	euclidean.dist = 
	  apply(knowledge.base, 1, function(row) { 
			sqrt(sum((row[1:(classId-1)] - query)^2)) })

  	w = weight(euclidean.dist, sigma)
	Y = knowledge.base[,classId]

	y = sum(w * Y) / sum(w)

	return (y)
}

dwnn.recursive <- function(dataset, time.delay, train.from=1, train.to=floor((nrow(dataset)*2)/3), sigma=0.5) {

	original.dataset = dataset
	ncol = ncol(dataset)

	for (to in (train.to+1):nrow(dataset)) {
		query = as.numeric(dataset[(to-time.delay),2:ncol])
		y = dwnn(dataset[train.from:(to-1),], query, sigma)
		dataset[to,] = c(query, y)
	}

	ret = list()
	ret$original.dataset = original.dataset
	ret$predicted.dataset = dataset

	return (ret)
}

concept.drift <- function(series, m, d, window.length=250, by=1) {

	dataset = embedd(series, m=m, d=d)
	divergence = c() #rep(NA, floor(window.length/by))

	for (window.start in seq(1, nrow(dataset)-2*window.length+1, by=by)) {
		cat("nrow: ", nrow(dataset), " Window start ", window.start, " predicting to ", (window.start+2*window.length-1),"\n")
		two.windows = dataset[window.start:(window.start+2*window.length-1),]
		result = dwnn.recursive(two.windows, time.delay=d, train.from=1, train.to=window.length)
		divergence = c(divergence, sum((result$original.dataset[,m] - result$predicted.dataset[,m])^2))
	}

	return (divergence)
}

#### TESTING ####
require(tseriesChaos)
test.sin <- function(sigma=0.5) {
	series = sin(2*pi*seq(0,9,len=1000))
	dataset = embedd(series, m=3, d=10)
	result = dwnn.recursive(dataset, time.delay=10, sigma=sigma)

	plot(result$original.dataset[,3])
	lines(result$predicted.dataset[,3], col=2)
}

#### DATASETS ####
experiment1 <- function(sigma=0.2104335, m=3, d=10, window.length=250, by=1) {

	series = c(sin(2*pi*seq(0,9,len=1000)), sin(3*pi*seq(0,9,len=1000)), 1.5*sin(3*pi*seq(0,9,len=1000)))

	div = concept.drift(series, m=m, d=d, window.length=window.length, by=by)
	div = c(rep(NA, floor(length(series)-length(div))/2), div, rep(NA, floor(length(series)-length(div))/2))

	pdf("experiment1.pdf")
	par(mfrow=c(2,1))
	plot(series, xlab="Time", ylab="Observation values", main="(a) Data stream")
	plot(div, xlab="Time", ylab="Squared-error values", main="(b) Prediction error")
	dev.off()

	ret = list()
	ret$series = series
	ret$div = div

	return (ret)
}

experiment2 <- function(sigma=0.00483608, m=2, d=1, window.length=250, by=1) {

	series = c()
	x = 0.5
	for (r in seq(2.5,4,len=2000)) {
		series = c(series, x)
		x = r*x*(1-x)
	}

	div = concept.drift(series, m=m, d=d, window.length=window.length, by=by)
	div = c(rep(NA, floor(length(series)-length(div))/2), div, rep(NA, floor(length(series)-length(div))/2))

	pdf("experiment2.pdf")
	par(mfrow=c(2,1))
	plot(series, xlab="Time", ylab="Observation values", main="(a) Data stream")
	plot(div, xlab="Time", ylab="Squared-error values", main="(b) Prediction error")
	dev.off()

	ret = list()
	ret$series = series
	ret$div = div

	return (ret)
}

experiment3 <- function(sigma=12.35516, m=3, d=10, window.length=250, by=1) {

	series = sunspot.month
	div = concept.drift(series, m=m, d=d, window.length=window.length, by=by)
	div = c(rep(NA, floor(length(series)-length(div))/2), div, rep(NA, floor(length(series)-length(div))/2))

	pdf("experiment3.pdf")
	par(mfrow=c(2,1))
	plot(series, xlab="Year", ylab="Observation values", main="(a) Data stream")
	plot(div, xlab="Time", ylab="Squared-error values", main="(b) Prediction error")
	dev.off()

	ret = list()
	ret$series = series
	ret$div = div

	return (ret)
}

experiment4 <- function(sigma=49.92655, m=3, d=1, window.length=250, by=1) {

	data = read.csv("SP500TR.csv")
	range = 4000:7566
	date = format(as.Date(data[range,1]), "%m-%Y")
	series  = data[range,6]

	div = concept.drift(series, m=m, d=d, window.length=window.length, by=by)
	div = c(rep(NA, floor(length(series)-length(div))/2), div, rep(NA, floor(length(series)-length(div))/2))

	pdf("experiment4.pdf")
	par(mfrow=c(2,1))
	seq = floor(seq(1, length(series), len = 8))
	plot(series, xlab="Month-Year", ylab="Observation values", main="(a) Data stream", xaxt="n")
	axis(1, at= seq, labels = date[seq])
	plot(div, xlab="Time", ylab="Squared-error values", main="(b) Prediction error")
	dev.off()

	ret = list()
	ret$series = series
	ret$div = div

	return (ret)
}

experiment5 <- function(sigma=0.1111036, m=3, d=1, window.length=250, by=1) {

	# http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt
	data = read.table("Land_and_Ocean_complete.txt")
	tmpDate = data[,1:2]
	date = 0
	for (i in 1:nrow(tmpDate))
	  date[i] = sprintf("%d-%d", tmpDate[i, 2], tmpDate[i, 1])
	series  = data[,3]

	div = concept.drift(series, m=m, d=d, window.length=window.length, by=by)
	div = c(rep(NA, floor(length(series)-length(div))/2), div, rep(NA, floor(length(series)-length(div))/2))

	pdf("experiment5.pdf")
	par(mfrow=c(2,1))
	seq = floor(seq(1, length(series), len = 8))
	plot(series, xlab="Month-Year", ylab="Observation values", main="(a) Data stream", xaxt="n")
  axis(1, at=seq, labels = date[seq])
	plot(div, xlab="Time", ylab="Squared-error values", main="(b) Prediction error")
	dev.off()

	ret = list()
	ret$series = series
	ret$div = div

	return (ret)
}


