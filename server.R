
rm(list=ls())
library(shiny)
library(Biobase)
source("functions.R")


load("C:/Users/FOLK/Documents/Work/Bioinformatics/Important R-images and cel files/2015-06-18 myfunctions.rdata")
load("C:/Users/FOLK/Documents/Work/Analysis/2014-12-17 LIBD databank/2015-03-19 pData for Lieber 746 samples.rdata")
path_RPKM<-"D:/dataBulk/Miscellanous/2015-02-24 LIBD expressionset/2015-02-23 DLPFC_PolyA normalizations and FPKM.rdata"
path_log2<-"D:/dataBulk/Miscellanous/2015-02-24 LIBD expressionset/2015-02-23 DLPFC_PolyA.rdata"
load(path_log2)
load("C:/Users/FOLK/Documents/Work/Bioinformatics/Important R-images and cel files/2015-02-24 simple annotation of seq LIEB ensembl.rdata")

# load("/home/ubuntu/data/2015-06-18 myfunctions.rdata")
# load("/home/ubuntu/data/2014-12-17 LIBD databank/2015-03-19 pData for Lieber 746 samples.rdata")
# path_RPKM<-"/home/ubuntu/data/2015-02-24 LIBD expressionset/2015-02-23 DLPFC_PolyA normalizations and FPKM.rdata"
# path_log2<-"/home/ubuntu/data/2015-02-24 LIBD expressionset/2015-02-23 DLPFC_PolyA.rdata"
# load(path_log2)
# load("/home/ubuntu/data/2015-02-24 simple annotation of seq LIEB ensembl.rdata")

pData(expressionset)<-pData[sampleNames(expressionset),]

expressionset[["SZ Antipsychotics"]]<-NA
expressionset[["SZ Antipsychotics"]][expressionset[["Age"]]<15 & expressionset[["Age"]]>0] <- "0 to 15 year old"
expressionset[["SZ Antipsychotics"]][expressionset[["Age"]]<0] <- "Pre-natal"
expressionset[["SZ Antipsychotics"]][expressionset[["Dx"]]%in%"Control" & expressionset[["Age"]] >= 15] <- "Healthy"
expressionset[["SZ Antipsychotics"]][expressionset[["Dx"]]%in%"Schizo" & expressionset[["Age"]] >= 15 & expressionset[["Antipsychotics"]] %in%"Negative"] <- "SZ non-med"
expressionset[["SZ Antipsychotics"]][expressionset[["Dx"]]%in%"Schizo" & expressionset[["Age"]] >= 15 & expressionset[["Antipsychotics"]] %in%"Positive"] <- "SZ med"
expressionset[["SZ Antipsychotics"]][expressionset[["Dx"]]%in%c("Bipolar","MDD") & expressionset[["Age"]] >= 15 ] <- "BP+MDD"
expressionset[["SZ Antipsychotics"]]<-factor(expressionset[["SZ Antipsychotics"]],levels=c("Pre-natal","0 to 15 year old","Healthy","SZ non-med","SZ med","BP+MDD"))




expressionset[["SZ Medication"]] <- rep("No",ncol(expressionset))
expressionset[["SZ Medication"]][grep("haldol",tolower(expressionset[["LastMedications"]]))]<-"Haloperidol"
expressionset[["SZ Medication"]][grep("risperidone",tolower(expressionset[["LastMedications"]]))]<-"Risperidone"
expressionset[["SZ Medication"]] <- factor(expressionset[["SZ Medication"]],levels=c("No","Haloperidol","Risperidone"))

# expressionset[["SZ halo/risp"]]<-NA
# expressionset[["SZ halo/risp"]][expressionset[["Age"]]<15 & expressionset[["Age"]]>0] <- "0 to 15 year old"
# expressionset[["SZ halo/risp"]][expressionset[["Age"]]<0] <- "Pre-natal"
# expressionset[["SZ halo/risp"]][expressionset[["Dx"]]%in%"Control" & expressionset[["Age"]] >= 15] <- "Healthy"
# expressionset[["SZ halo/risp"]][expressionset[["Dx"]]%in%c("Bipolar","MDD") & expressionset[["Age"]] >= 15 ] <- "BP+MDD"
# expressionset[["SZ halo/risp"]][expressionset[["Dx"]]%in%"Schizo" & expressionset[["Age"]] >= 15 & expressionset[["Antipsychotics"]] %in%"Negative"] <- "SZ non-med"
# expressionset[["SZ halo/risp"]][expressionset[["Dx"]]%in%"Schizo" & expressionset[["Age"]] >= 15 & expressionset[["Antipsychotics"]] %in%"Positive" & expressionset[["Haloperidol"]]%in% "Yes"] <- "SZ med H"
# expressionset[["SZ halo/risp"]][expressionset[["Dx"]]%in%"Schizo" & expressionset[["Age"]] >= 15 & expressionset[["Antipsychotics"]] %in%"Positive"  & expressionset[["Risperidone"]]%in% "Yes"] <- "SZ med R"
# expressionset[["SZ halo/risp"]]<-factor(expressionset[["SZ halo/risp"]],levels=c("Pre-natal","0 to 15 year old","Healthy","SZ non-med","SZ med","BP+MDD"))









fun_plot_groupwise_expression_data_20100830<-function (groups, main = "", ylab = "expression", xlim = NULL, 
																											 ylim = NULL, pointCol = "black", pointPch = 1, pCutoff = 0.05, 
																											 vLineMultiplier = 0.03, cexPValues = 1, plotAxis = TRUE, 
																											 plotN = TRUE, plotSummary = NULL, combinations = combn(names(groups), 
																											 																											 2), plotType = "dotplot", horizontalScatterSpacing = 0.05, 
																											 groupXPosition = seq(1, length(groups)), cex.axis = 1, las.axis = 1, 
																											 testType = "t.test", logYaxis = FALSE, verbose = TRUE) 
{
	if (class(plotType) != "character") 
		stop(paste("plotType must be of class character, not", 
							 class(plotType)))
	if (length(plotType) != 1) 
		stop("plotType must be of length 1")
	if (!plotType %in% c("boxplot", "dotplot", "ayasdiplot")) 
		stop("plotType must be either: 'boxplot' or 'dotplot' or 'ayasdiplot'")
	if (class(testType) != "character") 
		stop(paste("testType must be of class character, not", 
							 class(testType)))
	if (length(testType) != 1) 
		stop("testType must be of length 1")
	if (!testType %in% c("t.test", "paired t.test", "wilcox.test")) 
		stop("testType must be either: 't.test' or 'paired t.test' or 'wilcox.test'")
	if (class(groups) != "list") 
		stop(paste("groups must be of class list, not", class(groups)))
	for (entry in names(groups)) {
		if (class(groups[[entry]]) %in% c("integer")) {
			names <- names(groups[[entry]])
			groups[[entry]] <- as.numeric(groups[[entry]])
			names(groups[[entry]]) <- names
			if (verbose) 
				print(paste("The list entry", entry, "was given as integer, but was reformed to numeric"))
		}
		if (!class(groups[[entry]]) %in% c("NULL", "numeric")) 
			stop(paste("All list entries must be numeric vectors and", 
								 entry, "was not"))
		if (is.null(names(groups[[entry]]))) {
			if (length(groups[[entry]]) > 0) {
				names(groups[[entry]]) <- paste("entry", 1:length(groups[[entry]]))
			}
		}
	}
	if (plotType == "dotplot") {
		if (class(horizontalScatterSpacing) != "numeric") 
			stop(paste("horizontalScatterSpacing must be of class numeric, not", 
								 class(horizontalScatterSpacing)))
		if (length(horizontalScatterSpacing) != 1) 
			stop("horizontalScatterSpacing must be of length 1")
	}
	if (!class(groupXPosition) %in% c("integer", "numeric")) 
		stop(paste("groupXPosition must be of class numeric or integer, not", 
							 class(groupXPosition)))
	if (length(groupXPosition) != length(groups)) 
		stop("groupXPosition must be of the same length as the number of groups")
	if (!is.null(plotSummary)) {
		if (class(plotSummary) != "character") 
			stop(paste("plotSummary must be of class character, not", 
								 class(plotSummary)))
		if (length(plotSummary) != 1) 
			stop("plotSummary must be of length 1")
		if (!plotSummary %in% c("mean", "median")) 
			stop("plotSummary must be either: 'mean' or 'median'")
	}
	if (class(logYaxis) != "logical") 
		stop(paste("logYaxis must be of class logical, not", 
							 class(logYaxis)))
	if (logYaxis) {
		log <- "y"
	}
	else {
		log <- ""
	}
	if (!is.null(combinations)) {
		if (class(combinations) != "matrix") 
			stop(paste("combinations must be of class matrix, not", 
								 class(combinations)))
		if (nrow(combinations) %in% c(2, 3)) {
			if (!all(unique(c(combinations[1, ], combinations[2, 
																												])) %in% names(groups))) 
				stop("One or more of the entries in combinations were not found in the names of groups")
		}
		else {
			stop("combinations must have two or three lines")
		}
	}
	if (!class(pointCol) %in% c("list", "character")) {
		stop(paste("pointCol must be either of class character or list, not", 
							 class(pointCol)))
	}
	if (class(pointCol) == "list") {
		if (length(pointCol) != length(groups)) 
			stop(paste("when given as list pointCol must be either of the same lengths as groups (", 
								 length(groups), "), not ", length(pointCol), 
								 sep = ""))
		if (is.null(names(pointCol))) {
			names(pointCol) <- names(groups)
		}
		else {
			if (!all(names(pointCol) == names(groups))) 
				stop("If pointCol is given as named list, the names must be identical to the entries in groups")
		}
		for (entry in names(pointCol)) {
			if (class(pointCol[[entry]]) != "character") 
				stop(paste("The entry", entry, "from pointCol was not a character vector as is required when given pointCol as list"))
			if (length(pointCol[[entry]]) != length(groups[[entry]])) 
				stop(paste("The entry", entry, "from pointCol was of length", 
									 length(pointCol[[entry]]), "which was not the same as the corresponding groups entry length of", 
									 length(groups[[entry]])))
			if (is.null(names(pointCol[[entry]]))) {
				names(pointCol[[entry]]) <- names(groups[[entry]])
			}
			else {
				if (!all(names(pointCol[[entry]]) == names(groups[[entry]]))) 
					stop("If pointCol entries are given as named vector, the names must be identical to the entries in groups")
			}
		}
	}
	else {
		if (length(pointCol) != 1 & length(pointCol) != length(groups)) {
			stop(paste("when given as character vector pointCol must be either of length 1 or the same lengths as groups (", 
								 length(groups), "), not ", length(pointCol), 
								 sep = ""))
		}
		if (length(pointCol) == 1) {
			pointColVector <- rep(pointCol, length(groups))
		}
		else {
			pointColVector <- pointCol
		}
		names(pointColVector) <- names(groups)
		pointCol <- list()
		for (entry in names(groups)) {
			pointCol[[entry]] <- rep(pointColVector[entry], length(groups[[entry]]))
			names(pointCol[[entry]]) <- names(groups[[entry]])
		}
	}
	coloursFound <- unique(unlist(pointCol))
	hexadecimals <- coloursFound[grep("^#[0-9A-F]{6}", coloursFound)]
	missing <- unique(coloursFound[!(coloursFound %in% colors() | 
																	 	coloursFound %in% hexadecimals)])
	if (length(missing) > 0) {
		if (length(missing) == 1) {
			stop(paste("Didn't recognize the colour", missing))
		}
		else {
			stop(paste("Didn't recognize the colours:", paste(missing, 
																												collapse = ", ")))
		}
	}
	if (!class(pointPch) %in% c("list", "numeric", "integer")) {
		stop(paste("pointPch must be either of class integer or list, not", 
							 class(pointPch)))
	}
	if (class(pointPch) == "list") {
		if (length(pointPch) != length(groups)) 
			stop(paste("when given as list pointPch must be either of the same lengths as groups (", 
								 length(groups), "), not ", length(pointPch), 
								 sep = ""))
		if (is.null(names(pointPch))) {
			names(pointPch) <- names(groups)
		}
		else {
			if (!all(names(pointPch) == names(groups))) 
				stop("If pointPch is given as named list, the names must be identical to the entries in groups")
		}
		for (entry in names(pointPch)) {
			if (class(pointPch[[entry]]) %in% "numeric") 
				pointPch[[entry]] <- as.integer(pointPch[[entry]])
			if (class(pointPch[[entry]]) != "integer") 
				stop(paste("The entry", entry, "from pointPch was not an integer vector as is required when given pointPch as list. It was a", 
									 class(pointPch[[entry]])))
			if (length(pointPch[[entry]]) != length(groups[[entry]])) 
				stop(paste("The entry", entry, "from pointPch was of length", 
									 length(pointPch[[entry]]), "which was not the same as the corresponding groups entry length of", 
									 length(groups[[entry]])))
			if (is.null(names(pointPch[[entry]]))) {
				names(pointPch[[entry]]) <- names(groups[[entry]])
			}
			else {
				if (!all(names(pointPch[[entry]]) == names(groups[[entry]]))) 
					stop("If pointPch entries are given as named vector, the names must be identical to the entries in groups")
			}
		}
	}
	else {
		if (class(pointPch) %in% "numeric") 
			pointPch <- as.integer(pointPch)
		if (length(pointPch) != 1 & length(pointPch) != length(groups)) {
			stop(paste("when given as vector pointPch must be either of length 1 or the same lengths as groups (", 
								 length(groups), "), not ", length(pointPch), 
								 sep = ""))
		}
		if (length(pointPch) == 1) {
			pointPchVector <- rep(pointPch, length(groups))
		}
		else {
			pointPchVector <- pointPch
		}
		names(pointPchVector) <- names(groups)
		pointPch <- list()
		for (entry in names(groups)) {
			pointPch[[entry]] <- rep(pointPchVector[entry], length(groups[[entry]]))
			names(pointPch[[entry]]) <- names(groups[[entry]])
		}
	}
	pchFound <- unique(unlist(pointPch))
	missing <- unique(pchFound[!pchFound %in% 0:25])
	if (length(missing) > 0) {
		stop(paste("Didn't recognize the pch", missing))
	}
	for (entry in names(groups)) {
		pointPch[[entry]] <- pointPch[[entry]][!is.na(groups[[entry]])]
		pointCol[[entry]] <- pointCol[[entry]][!is.na(groups[[entry]])]
		groups[[entry]] <- groups[[entry]][!is.na(groups[[entry]])]
	}
	if (log != "y") {
		vLine <- (max(unlist(groups), na.rm = TRUE) - min(unlist(groups), 
																											na.rm = TRUE)) * vLineMultiplier
	}
	else {
		vLine <- max(unlist(groups), na.rm = TRUE) * vLineMultiplier * 
			20
	}
	if (is.null(ylim)) {
		if (log != "y") {
			ylim <- c(min(unlist(groups), na.rm = TRUE) - vLine, 
								max(unlist(groups), na.rm = TRUE) + 10 * vLine)
		}
		else {
			ylim <- c(min(unlist(groups), na.rm = TRUE) * 0.9, 
								max(unlist(groups), na.rm = TRUE) + 10 * vLine)
		}
	}
	else {
		if (class(ylim) != "numeric") 
			stop(paste("ylim must be of class numeric, not", 
								 class(ylim)))
		if (length(ylim) != 2) 
			stop("ylim must be of length 2")
	}
	if (is.null(xlim)) {
		xlim <- c(1 - (length(groups) * 0.15), length(groups) * 
								1.15)
	}
	else {
		if (class(xlim) != "numeric") 
			stop(paste("xlim must be of class numeric, not", 
								 class(xlim)))
		if (length(xlim) != 2) 
			stop("xlim must be of length 2")
	}
	if (plotType == "boxplot") {
		categoriesCharacter <- vector()
		expression <- vector()
		for (level in names(groups)) {
			categoriesCharacter <- c(categoriesCharacter, rep(level, 
																												length(groups[[level]])))
			expression <- c(expression, groups[[level]])
		}
		categories <- factor(x = categoriesCharacter, levels = names(groups))
		plot.default(NULL, xaxt = "n", main = main, ylab = ylab, 
								 ylim = ylim, xlim = xlim, xlab = "", log = log)
		boxplot(expression ~ categories, add = TRUE, xaxt = "n", 
						at = groupXPosition, xlab = "")
	}
	if (plotType == "dotplot") {
		suppressWarnings(plot(NULL, xaxt = "n", ylab = ylab, 
													xlab = "", xlim = xlim, ylim = ylim, main = main, 
													log = log))
		NumberOfDotsOfCexOneInOneLane <- 50
		for (x in 1:length(groups)) {
			level <- names(groups)[x]
			if (length(groups[[level]]) > 0) {
				distribution <- hist(groups[[level]], plot = FALSE, 
														 breaks = NumberOfDotsOfCexOneInOneLane)
				bins <- c(ylim[1], distribution[["breaks"]], 
									ylim[2])
				for (i in 1:(length(bins) - 1)) {
					ys <- groups[[level]][bins[i] < groups[[level]] & 
																	groups[[level]] <= bins[i + 1]]
					if (length(ys) > 0) {
						for (j in 1:length(ys)) {
							xHere <- groupXPosition[x] - (length(ys)/2) * 
								horizontalScatterSpacing + j * horizontalScatterSpacing - 
								horizontalScatterSpacing/2
							points(y = ys[j], x = xHere, col = pointCol[[level]][names(ys)[j]], 
										 pch = pointPch[[level]][names(ys)[j]])
						}
					}
				}
			}
			if (!is.null(plotSummary)) {
				if (plotSummary == "median") {
					lineY <- median(groups[[x]], na.rm = T)
				}
				if (plotSummary == "mean") {
					lineY <- mean(groups[[x]], na.rm = T)
				}
				width <- ((max(groupXPosition, na.rm = T) - min(groupXPosition, 
																												na.rm = T))/length(groups)) * 0.4
				lines(x = c(groupXPosition[x] - width, groupXPosition[x] + 
											width), y = c(lineY, lineY))
			}
		}
	}
	if (plotType == "ayasdiplot") {
		ayasdiPoints <- function(x, y, innerRadius = 0.02, outerRadius = 0.05, 
														 col = "red", complexity = 10) {
			red <- col2rgb(col)[1]
			green <- col2rgb(col)[2]
			blue <- col2rgb(col)[3]
			hue <- rgb2hsv(red, green, blue)[1]
			saturation <- rgb2hsv(red, green, blue)[2]
			value <- rgb2hsv(red, green, blue)[3]
			darkCols <- hsv(hue, saturation, value * seq(0.3, 
																									 0.7, 0.05 * (10/complexity)))
			radiiToShade0 <- seq(outerRadius * 1.3, outerRadius, 
													 length.out = length(darkCols))
			for (i in 1:length(darkCols)) {
				symbols(x = x, y = y, circles = radiiToShade0[i], 
								inches = FALSE, add = TRUE, bg = darkCols[i], 
								fg = NULL)
			}
			halfWayRadius <- mean(c(innerRadius, outerRadius))
			radiiToShade1 <- seq(outerRadius, halfWayRadius, 
													 length.out = length(darkCols))
			for (i in 1:length(darkCols)) {
				symbols(x = x, y = y, circles = radiiToShade1[i], 
								inches = FALSE, add = TRUE, bg = darkCols[length(darkCols) - 
																														i], fg = NULL)
			}
			radiiToShade2 <- seq(halfWayRadius, innerRadius, 
													 length.out = length(darkCols))
			for (i in 1:length(darkCols)) {
				symbols(x = x, y = y, circles = radiiToShade2[i], 
								inches = FALSE, add = TRUE, bg = darkCols[i], 
								fg = NULL)
			}
			symbols(x = x, y = y, circles = outerRadius, inches = FALSE, 
							add = TRUE, fg = col, bg = NULL)
			symbols(x = x, y = y, circles = innerRadius, inches = FALSE, 
							add = TRUE, fg = NULL, bg = col)
		}
		plot(NULL, xaxt = "n", ylab = ylab, xlab = "", xlim = xlim, 
				 ylim = ylim, main = main)
		edgeCutoff <- 2
		symbols(mean(xlim), mean(ylim), rectangles = matrix(c(xlim[2] - 
																														xlim[1] + edgeCutoff, ylim[2] - ylim[1] + edgeCutoff), 
																												nrow = 1), inches = FALSE, add = TRUE, fg = NULL, 
						bg = "black")
		positionMatrix <- data.frame(x = 0, y = 0, col = "red", 
																 pch = 19, stringsAsFactors = FALSE)[0, ]
		NumberOfDotsOfCexOneInOneLane <- 5
		for (x in 1:length(groups)) {
			level <- names(groups)[x]
			if (length(groups[[level]]) > 0) {
				distribution <- hist(groups[[level]], plot = FALSE, 
														 breaks = NumberOfDotsOfCexOneInOneLane)
				bins <- c(ylim[1], distribution[["breaks"]], 
									ylim[2])
				for (i in 1:(length(bins) - 1)) {
					ys <- groups[[level]][bins[i] < groups[[level]] & 
																	groups[[level]] <= bins[i + 1]]
					if (length(ys) > 0) {
						for (j in 1:length(ys)) {
							xHere <- groupXPosition[x] - (length(ys)/2) * 
								horizontalScatterSpacing + j * horizontalScatterSpacing - 
								horizontalScatterSpacing/2
							positionMatrix[paste(x, i, j), "x"] <- xHere
							positionMatrix[paste(x, i, j), "y"] <- ys[j]
							positionMatrix[paste(x, i, j), "col"] <- pointCol[[level]][names(ys)[j]]
							positionMatrix[paste(x, i, j), "pch"] <- pointPch[[level]][names(ys)[j]]
						}
					}
				}
			}
		}
		combHere <- combn(1:nrow(positionMatrix), 2)
		for (i in 1:ncol(combHere)) {
			x1 <- positionMatrix[combHere[1, i], "x"]
			y1 <- positionMatrix[combHere[1, i], "y"]
			x2 <- positionMatrix[combHere[2, i], "x"]
			y2 <- positionMatrix[combHere[2, i], "y"]
			dist <- ((x1 - x2)^2 + (y1 - y2)^2)^0.5
			if (dist < 0.4) {
				lines(x = c(x1, x2), y = c(y1, y2), col = "grey30")
			}
		}
		for (i in 1:nrow(positionMatrix)) {
			ayasdiPoints(y = positionMatrix[i, "y"], x = positionMatrix[i, 
																																	"x"], col = positionMatrix[i, "col"], outerRadius = 0.025 + 
									 	(positionMatrix[i, "pch"]/25) * 0.06)
		}
	}
	if (plotAxis) {
		axisLabels <- vector()
		for (level in names(groups)) {
			if (plotN) {
				axisLabels <- c(axisLabels, paste(level, "\nn=", 
																					length(groups[[level]]), sep = ""))
			}
			else {
				axisLabels <- c(axisLabels, level)
			}
		}
		if (length(grep("\\n", axisLabels)) > 0) {
			axisPadj = 0.5
		}
		else {
			axisPadj = 0
		}
		axis(1, at = groupXPosition, labels = axisLabels, padj = axisPadj, 
				 cex.axis = cex.axis, las = las.axis)
	}
	if (!is.null(combinations)) {
		significantCount <- 0
		for (i in 1:ncol(combinations)) {
			if (nrow(combinations) == 3) {
				p <- combinations[3, i]
				p <- suppressWarnings(as.numeric(p))
				if (!is.na(p)) {
					p <- signif(p, 3)
				}
			}
			else if (testType == "t.test") {
				p <- try(signif(t.test(groups[[combinations[1, 
																										i]]], groups[[combinations[2, i]]])[[3]], 3), 
								 silent = TRUE)
			}
			else if (testType == "paired t.test") {
				if (length(groups[[combinations[1, i]]]) != length(groups[[combinations[2, 
																																								i]]])) {
					stop(paste("A paired t.test was requested but group '", 
										 combinations[1, i], "' and '", combinations[2, 
										 																						i], "' did not have the same length", sep = ""))
				}
				pairing <- paste(paste(names(groups[[combinations[1, 
																													i]]]), names(groups[[combinations[2, i]]]), 
															 sep = " and "), collapse = ", ")
				if (verbose) 
					print(paste("Paired T-test was calculated for these pairings:", 
											pairing))
				p <- try(signif(t.test(groups[[combinations[1, 
																										i]]], groups[[combinations[2, i]]], paired = TRUE)[[3]], 
												3), silent = TRUE)
			}
			else if (testType == "wilcox.test") {
				p <- try(signif(wilcox.test(groups[[combinations[1, 
																												 i]]], groups[[combinations[2, i]]])[[3]], 3), 
								 silent = TRUE)
			}
			else {
				stop("Test not recognized")
			}
			if (class(p) != "try-error" & !is.na(p)) {
				if (p < pCutoff) {
					xPos <- c(groupXPosition[match(combinations[1, 
																											i], names(groups))], groupXPosition[match(combinations[2, 
																																																						 i], names(groups))])
					yMax <- ylim[2] - vLine - significantCount * 
						vLine * 3
					lines(x = xPos, y = c(yMax, yMax))
					lines(x = c(xPos[1], xPos[1]), y = c(yMax, 
																							 yMax - vLine))
					lines(x = c(xPos[2], xPos[2]), y = c(yMax, 
																							 yMax - vLine))
					text(x = mean(c(xPos[1], xPos[2])), y = yMax + 
							 	vLine, labels = paste("P =", p), cex = cexPValues)
					significantCount <- significantCount + 1
				}
			}
		}
	}
}



fun_plot_gene_vs_gene_20100416<-function(expressionset,x_gene,y_genes,correlation_list=NULL,pointCol=NULL,annotation=NULL){
	#function to plot two genes against each other. Takes two genes (x_gene and y_gene) and a dataset (x_dataset)
	#Optional arguments:
	#	annotation - a list of annotation which is a dataframe with the featurenames as rownames, and a column called genesymbol (should contain genesymbol), and a column called. 
	#									genename (which can contain other stuff - such as refseq or whatever).
	#	correlation_list - a list of correlations. Speed up slightly as calculations can be taken from here.
	
	library(Biobase)
	
	if(!class(expressionset)%in%"ExpressionSet")stop(paste("expressionset must be of class ExpressionSet, not",class(expressionset)))
	if(ncol(expressionset)<2)stop(paste("expressionset must have more than 1 sample"))
	
	if(class(x_gene)!="character")stop(paste("x_gene given must be of class character not",class(x_gene)))
	if(length(x_gene)!=1)stop(paste("x_gene given must be of length 1, not ",length(x_gene)))
	if(class(y_genes)!="character")stop(paste("x_gene given must be of class character not",class(y_genes)))
	if(!x_gene %in% featureNames(expressionset))stop(paste("x_gene",x_gene,"was not found in the expressionset"))
	if(!all(y_genes %in% featureNames(expressionset)))stop(paste("some y_genes were not found in the expressionset"))
	
	
	if(!is.null(annotation)){
		if(class(annotation)!="data.frame")stop(paste("annotation given must be of class data.frame not",class(annotation)))
		if("genetitle"%in%colnames(annotation) & !"genename"%in%colnames(annotation)){
			colnames(annotation)[colnames(annotation)%in%"genetitle"]<-"genename"	
		}
		if(!all(c("genesymbol","genename")%in%colnames(annotation)))stop("annotation must contain the colnames genesymbol and genename")
		if(!all(c(x_gene,y_genes)%in%rownames(annotation))){
			missing<-c(x_gene,y_genes)[!c(x_gene,y_genes)%in%rownames(annotation)]
			stop(paste("The following probesets where not found in the annotation:",paste(missing,collapse=", ")))
		}
	}
	
	if(!x_gene%in%featureNames(expressionset))stop(paste("The x_gene:",x_gene,"was not found in the expressionset"))
	
	if(!all(y_genes%in%featureNames(expressionset))){
		missing<-y_genes[!y_genes%in%featureNames(expressionset)]
		stop(paste("The following y_genes where not found in the expressionset:",paste(missing,collapse=", ")))
	}
	
	if(!is.null(pointCol)){
		if(class(pointCol)!="character")stop(paste("If given, pointCol must be of class character not",class(pointCol)))
		if(length(pointCol) != 1 & length(pointCol) != ncol(expressionset))stop(paste("If given, pointCol must be either of length 1 or of the same length as expressionset, not",length(pointCol)))
		if(!any(pointCol%in%colors())){
			missingCol<-unique(pointCol[!pointCol%in%colors()])
			stop(paste("Didn't recognize the colour",paste(missingCol,collapse=", ")))
		}
	}else{
		pointCol="black"	
	}
	
	if(is.null(correlation_list)){
		correlation_list<-fun_calculate_gene_to_gene_correlation_20080409(expressionset,x_gene,probesets=y_genes)
	}
	
	
	for(y_gene in y_genes){
		if(is.null(annotation)){
			y_genesymbol = ""
			x_genesymbol = ""
			y_genename = ""
			x_genename = ""
		}else{
			y_genesymbol=annotation[y_gene,"genesymbol"]
			x_genesymbol=annotation[x_gene,"genesymbol"]
			y_genename = annotation[y_gene,"genename"]
			x_genename = annotation[x_gene,"genename"]
		}
		plot.default(
			type="p",
			y=exprs(expressionset[y_gene,]),
			x=exprs(expressionset[x_gene,]),
			ylab=paste(y_gene,": ",y_genesymbol,sep=""),
			xlab=paste(x_gene,": ",x_genesymbol,sep=""),
			main=list(paste("Expression plot of",x_gene,"varying with",y_gene),cex=0.7),
			sub=list(paste("correlation: ",signif(correlation_list[y_gene,"correlation"],3),"p-value",signif(correlation_list[y_gene,"p-value"],3),sep=" "),cex=0.7),
			col = pointCol
		)
		
		mtext(paste("       on Y-axis: ",y_genesymbol," - ",y_genename,sep=""),adj=0,padj=-1.4,cex=0.7)
		mtext(paste("       on X-axis: ",x_genesymbol," - ",x_genename,sep=""),adj=0,padj=-0.2,cex=0.7)
	}
}




fun_calculate_gene_to_gene_correlation_20080409<-function(expressionset,gene,correlation_type="pearson",probesets=NULL){
	# Takes a dataset and a probeset id, and outputs a list of genes with the best correlations and also p-values for it.
	# Not implemented yet -- do other correlation types with the correlation_type parameter
	
	if(!class(expressionset)%in%"ExpressionSet")stop(paste("expressionset must be of class ExpressionSet, not",class(expressionset)))
	if(ncol(expressionset)<2)stop(paste("expressionset must have more than 1 sample"))
	
	if(!correlation_type%in%c("pearson","spearman"))stop(paste("The correlation type",correlation_type,"is not recognised"))
	
	if(class(gene)!="character")stop(paste("gene given must be of class character not",class(gene)))
	if(length(gene)!=1)stop(paste("gene given must be of length 1, not ",length(gene)))
	if(!gene %in% featureNames(expressionset))stop(paste("gene",gene,"was not found in the expressionset"))
	
	if(is.null(probesets)){
		probesets<-featureNames(expressionset)	
	}else{
		if(class(probesets)!="character")stop(paste("probesets given must be of class character not",class(probesets)))
		if(!all(probesets%in%featureNames(expressionset))){
			missing<-probesets[!probesets%in%featureNames(expressionset)]
			stop(paste("Didn't find the following probesets in the given expressionset:",paste(missing,collapse=", ")))
		}
	}
	
	if(length(probesets)>1){
		find_pearson_correlation<-function(x,gene_of_interest_data){
			cor_result<-cor.test(gene_of_interest_data,x,method=correlation_type)
			c(cor_result$p.value,cor_result$estimate)
		}
		
		
		correlations<-t(apply(exprs(expressionset)[probesets,],1,find_pearson_correlation,gene_of_interest_data=exprs(expressionset)[gene,]))
		colnames(correlations)<-c("p-value","correlation")
		correlations<-correlations[order(abs(correlations[,"correlation"]),decreasing=TRUE),]
	}else{#when there is only one probeset
		cor_result<-cor.test(exprs(expressionset)[probesets,],exprs(expressionset)[gene,],method=correlation_type)
		correlations<-data.frame("p-value"=cor_result$p.value,"correlation"=cor_result$estimate,row.names=probesets)
		colnames(correlations)<-c("p-value","correlation")
		
	}
	return(correlations)
}



fun_plot_genes_vs_clinical_20110209<-function (expressionset, probesets, label, annotation = NULL, 
																							 colourBy = NULL, pchBy = NULL, connectBy = NULL, horizontalScatterSpacing = 0.02, 
																							 plotSummary = "median", doStatistics = FALSE, mtexts = c("Category", 
																							 																												 "Data set", "Annotation", "X-labels", "n"), groupXPosition = NULL, 
																							 ylab = "expression", xlab = label, verbose = TRUE, log = "", 
																							 ...) 
{
	require(Biobase)
	if (!class(expressionset)[1] %in% c("ProbeLevelSet", "ExpressionSet")) 
		stop(paste("expressionset must be of class ExpressionSet and not", 
							 class(expressionset)))
	if (class(probesets) != "character") 
		stop(paste("probesets must be of class character and not", 
							 class(probesets)))
	if (class(label) != "character") 
		stop(paste("label must be of class character and not", 
							 class(label)))
	if (length(label) != 1) 
		stop(paste("label must be of length 1"))
	if (!label %in% colnames(pData(expressionset))) 
		stop("The given label was not found in the expressionset")
	if (!class(expressionset[[label]]) %in% c("factor", "numeric", 
																						"integer", "Date")) 
		stop(paste("The label", label, "was of class", class(expressionset[[label]]), 
							 "- it must be either factor, numeric, integer or Date"))
	if (class(expressionset[[label]]) %in% "factor") {
		if ("" %in% expressionset[[label]]) 
			stop(paste("The label", label, "contained empty string entries (i.e. ''). This is not allowed as it causes failure in R-lists"))
	}
	if (!is.null(annotation)) {
		if (class(annotation) != "data.frame") 
			stop(paste("annotation given must be of class data.frame not", 
								 class(annotation)))
		if ("genetitle" %in% colnames(annotation) & !"genename" %in% 
				colnames(annotation)) {
			colnames(annotation)[colnames(annotation) %in% "genetitle"] <- "genename"
		}
		if (!all(c("genename", "genesymbol") %in% colnames(annotation))) {
			stop("The given annotation file missed either the genename column or the genesymbol column")
		}
	}
	else {
		if ("genesymbol" %in% colnames(pData(featureData(expressionset)))) {
			if ("genetitle" %in% colnames(pData(featureData(expressionset)))) {
				annotation <- data.frame(row.names = featureNames(expressionset), 
																 genesymbol = pData(featureData(expressionset))[, 
																 																							 "genesymbol"], genename = pData(featureData(expressionset))[, 
																 																							 																														"genetitle"])
			}
			else if ("genename" %in% colnames(pData(featureData(expressionset)))) {
				annotation <- data.frame(row.names = featureNames(expressionset), 
																 genesymbol = pData(featureData(expressionset))[, 
																 																							 "genesymbol"], genename = pData(featureData(expressionset))[, 
																 																							 																														"genename"])
			}
			else {
				annotation <- data.frame(row.names = featureNames(expressionset), 
																 genesymbol = pData(featureData(expressionset))[, 
																 																							 "genesymbol"], genename = rep("", nrow(expressionset)))
			}
		}
		else {
			annotation <- data.frame(row.names = vector(), genesymbol = vector(), 
															 genename = vector())
		}
	}
	if (class(mtexts) != "character") 
		stop(paste("mtexts must be of class character and not", 
							 class(mtexts)))
	allowed <- c("Category", "Data set", "Annotation", "X-labels", 
							 "n", "")
	if (any(!mtexts %in% allowed)) 
		stop(paste("Only the following X-labels are allowed:", 
							 paste(allowed, collapse = ", ")))
	if (class(ylab) != "character") 
		stop(paste("ylab must be of class character and not", 
							 class(ylab)))
	if (class(xlab) != "character") 
		stop(paste("xlab must be of class character and not", 
							 class(xlab)))
	if (length(ylab) != 1) 
		stop(paste("ylab must be of length 1, not", length(ylab)))
	if (length(xlab) != 1) 
		stop(paste("xlab must be of length 1, not", length(xlab)))
	if (!is.null(log)) {
		if (class(log) != "character") 
			stop(paste("log must be of class character and not", 
								 class(log)))
		if (length(log) != 1) 
			stop(paste("log must be of length 1, not", length(log)))
	}
	if (class(expressionset[[label]]) %in% "factor") {
		if (is.null(groupXPosition)) {
			groupXPosition <- seq(1, length(levels(expressionset[[label]])))
		}
		else {
			if (class(groupXPosition) != "numeric") {
				stop(paste("If given, groupXPosition must be of class numeric, not", 
									 class(groupXPosition)))
			}
			if (length(levels(expressionset[[label]])) != length(groupXPosition)) 
				stop("If given, groupXPosition must be of the same length as the number of levels in the label")
		}
	}
	if (!is.null(colourBy)) {
		if (class(colourBy) != "character") 
			stop(paste("colourBy must be of class character, not", 
								 class(colourBy)))
		if (length(colourBy) != 1) 
			stop(paste("colourBy must be of length 1, not", length(colourBy)))
		if (!colourBy %in% colnames(pData(expressionset))) {
			if (colourBy %in% colors()) {
				expressionset[["colourBy"]] <- "all"
				colours <- colourBy
				names(colours) <- "all"
			}
			else {
				stop(paste("colourBy", colourBy, "was not found as an entry in the pData of the expressionset (or in 'colors()' for direct colouring)"))
			}
		}
		else {
			if (!class(expressionset[[colourBy]]) %in% c("numeric", 
																									 "factor")) 
				stop(paste("colourBy", colourBy, "was found in expressionset, but it was a", 
									 class(expressionset[[colourBy]]), "not a factor entry"))
			if (class(expressionset[[colourBy]]) %in% c("factor")) {
				colours <- c("green", "firebrick4", "darkorange1", 
										 "yellow2", "red", "blue", "orange", "darkgreen", 
										 "dodgerblue", "#8DD3C7", "#BEBADA", "#FB8072", 
										 "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", 
										 "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", 
										 "#FFFFB3")
				if (length(levels(expressionset[[colourBy]])) > 
						length(colours)) 
					stop(paste("colourBy", colourBy, "was found in expressionset, but had more than", 
										 length(colours), "levels, which is not allowed"))
				names(colours) <- levels(expressionset[[colourBy]])
				expressionset[["colourBy"]] <- expressionset[[colourBy]]
			}
			else {
				range <- range(expressionset[[colourBy]], na.rm = TRUE)
				bins <- seq(from = range[1], to = range[2], by = (range[2] - 
																														range[1])/100)
				pal <- rainbow(101, v = 0.5, start = 0.98, end = 0.2)
				colours <- vector()
				expressionset[["colourBy"]] <- as.factor(expressionset[[colourBy]])
				for (value in levels(expressionset[["colourBy"]])) {
					if (!is.na(value)) {
						colours <- c(colours, pal[sum(as.numeric(value) < 
																						bins) + 1])
					}
				}
				names(colours) <- levels(expressionset[["colourBy"]])
			}
		}
	}
	if (!is.null(pchBy)) {
		if (length(pchBy) != 1) 
			stop(paste("pchBy must be of length 1, not", length(pchBy)))
		if (class(pchBy) != "character") {
			if (class(pchBy) == "numeric") {
				pchBy <- as.integer(pchBy)
			}
			if (!class(pchBy) == "integer") {
				stop(paste("pchBy must be of class character, not", 
									 class(pchBy), "- alternatively an integer specifying overall pch type"))
			}
			else if (!pchBy %in% 1:25) {
				stop(paste("if pchBy is an integer it must be between 1 and 25 - see '?points'"))
			}
			else {
				expressionset[["pchBy"]] <- "all"
				pch <- pchBy
				names(pch) <- "all"
				pchBy <- "pchBy"
			}
		}
		else {
			if (!pchBy %in% colnames(pData(expressionset))) 
				stop(paste("pchBy", pchBy, "was not found as an entry in the pData of the expressionset"))
			if (!class(expressionset[[pchBy]]) %in% c("factor")) 
				stop(paste("pchBy", pchBy, "was found in expressionset, but it was a", 
									 class(expressionset[[pchBy]]), "not a factor entry"))
			pch <- c(15, 17, 19, 18, 1, 2, 0, 5, 6, 7:14)
			if (length(levels(expressionset[[pchBy]])) > length(pch)) 
				stop(paste("pchBy", pchBy, "was found in expressionset, but had more than", 
									 length(pch), "levels, which is not allowed"))
			names(pch) <- levels(expressionset[[pchBy]])
		}
	}
	if (!is.null(connectBy)) {
		if (!connectBy %in% colnames(pData(expressionset))) 
			stop(paste("connectBy", connectBy, "was not found in pData of expressionset"))
		if (!class(expressionset[[connectBy]]) %in% c("character", 
																									"factor")) 
			stop(paste("connectBy variable", connectBy, "was not of class factor or character"))
		if (any(sum(table(expressionset[[connectBy]]) > 10))) {
			tooMany <- names(table(expressionset[[connectBy]]))[table(expressionset[[connectBy]]) > 
																														5]
			stop(paste("These entries had more than 5 duplicates in connectBy:", 
								 paste(tooMany, collapse = ", ")))
		}
	}
	if (verbose) {
		print(paste("Plotting: now plotting", length(probesets), 
								"from the set", experimentData(expressionset)@title, 
								"at", label))
	}
	for (probeset in probesets) {
		if (class(expressionset[[label]]) %in% "factor") {
			groups <- list()
			for (group in levels(pData(expressionset)[, label])) {
				groups[[group]] <- exprs(expressionset)[probeset, 
																								pData(expressionset)[, label] %in% group]
			}
			if (doStatistics) {
				combinations = combn(names(groups), 2)
			}
			else {
				combinations = NULL
			}
			if (!is.null(colourBy)) {
				pointCol <- list()
				for (group in levels(pData(expressionset)[, label])) {
					pointCol[[group]] <- colours[pData(expressionset)[pData(expressionset)[, 
																																								 label] %in% group, "colourBy"]]
					pointCol[[group]][is.na(pointCol[[group]])] <- "grey"
					names(pointCol[[group]]) <- NULL
				}
			}
			else {
				pointCol <- "black"
			}
			if (!is.null(pchBy)) {
				pointPch <- list()
				for (group in levels(pData(expressionset)[, label])) {
					pointPch[[group]] <- pch[pData(expressionset)[pData(expressionset)[, 
																																						 label] %in% group, pchBy]]
					pointPch[[group]][is.na(pointPch[[group]])] <- 20
					names(pointPch[[group]]) <- NULL
				}
			}
			else {
				if (length(pointCol) == 1 & class(pointCol) == 
						"character") {
					pointPch <- 1
				}
				else {
					pointPch <- 19
				}
			}
			if (length(levels(expressionset[[label]])) > 6) {
				cex.axis = 0.4
			}
			else {
				cex.axis = 0.7
			}
			if ("n" %in% mtexts) {
				plotN <- TRUE
			}
			else {
				plotN <- FALSE
			}
			if ("X-labels" %in% mtexts) {
				plotAxis <- TRUE
			}
			else {
				plotAxis <- FALSE
				plotN <- FALSE
			}
			fun_plot_groupwise_expression_data_20100830(groups = groups, 
																									main = "", ylab = ylab, cexPValues = 0.7, plotAxis = plotAxis, 
																									plotN = plotN, combinations = combinations, plotType = "dotplot", 
																									cex.axis = 0.7, horizontalScatterSpacing = horizontalScatterSpacing, 
																									pointCol = pointCol, pointPch = pointPch, plotSummary = plotSummary, 
																									groupXPosition = groupXPosition, logYaxis = length(grep("y", 
																																																					log)) == 1, ...)
			if (!is.null(connectBy)) {
				for (duplicatedSample in unique(expressionset[[connectBy]])) {
					whichEntries <- which(expressionset[[connectBy]] %in% 
																	duplicatedSample)
					if (length(whichEntries) > 1) {
						pairings <- combn(whichEntries, 2)
						for (i in 1:ncol(pairings)) {
							x1 <- groupXPosition[which(levels(expressionset[[label]]) %in% 
																				 	expressionset[[label]][pairings[1, i]])]
							x2 <- groupXPosition[which(levels(expressionset[[label]]) %in% 
																				 	expressionset[[label]][pairings[2, i]])]
							y1 <- exprs(expressionset)[probeset, pairings[1, 
																														i]]
							y2 <- exprs(expressionset)[probeset, pairings[2, 
																														i]]
							if (all(sapply(list(x1, x2, y1, y2), length) == 
											1)) {
								lines(x = c(x1, x2), y = c(y1, y2), lty = "dotted", 
											col = "grey")
							}
						}
					}
				}
			}
		}
		if (class(expressionset[[label]]) %in% c("integer", "numeric", 
																						 "Date")) {
			if (!is.null(pchBy)) {
				pointCol <- colours[pData(expressionset)[, "colourBy"]]
				pointCol[is.na(pointCol)] <- "grey"
			}
			else {
				pointCol <- "black"
			}
			if (!is.null(pchBy)) {
				pointPch <- pch[pData(expressionset)[, pchBy]]
				pointPch[is.na(pointPch)] <- 20
			}
			else {
				if (length(pointCol) == 1 & class(pointCol) == 
						"character") {
					pointPch <- 1
				}
				else {
					pointPch <- 19
				}
			}
			plot.default(type = "p", x = pData(expressionset)[, 
																												label], y = exprs(expressionset)[probeset, ], 
									 xlab = xlab, ylab = ylab, main = "", cex = 1, 
									 col = pointCol, pch = pointPch, log = log, ...)
			if (doStatistics & !class(expressionset[[label]]) %in% 
					"Date") {
				correlation <- signif(cor.test(exprs(expressionset)[probeset, 
																														], expressionset[[label]])[[4]], 2)
				mtext(paste("Pearson correlation coefficient:", 
										correlation), padj = 7, side = 1, cex = 0.7, 
							adj = 0)
			}
			if (!is.null(connectBy)) {
				for (duplicatedSample in unique(expressionset[[connectBy]])) {
					whichEntries <- which(expressionset[[connectBy]] %in% 
																	duplicatedSample)
					if (length(whichEntries) > 1) {
						pairings <- combn(whichEntries, 2)
						for (i in 1:ncol(pairings)) {
							x1 <- expressionset[[label]][pairings[1, 
																										i]]
							x2 <- expressionset[[label]][pairings[2, 
																										i]]
							y1 <- exprs(expressionset)[probeset, pairings[1, 
																														i]]
							y2 <- exprs(expressionset)[probeset, pairings[2, 
																														i]]
							lines(x = c(x1, x2), y = c(y1, y2), lty = "dotted", 
										col = "grey")
						}
					}
				}
			}
		}
		if ("Category" %in% mtexts) {
			mtext(paste("Category:", label), padj = -1.4, cex = 0.7, 
						adj = 0)
		}
		if ("Data set" %in% mtexts) {
			mtext(paste("Data set: ", expressionset@experimentData@title, 
									sep = ""), padj = -0.2, cex = 0.7, adj = 0)
		}
		if ("Annotation" %in% mtexts) {
			if (probeset %in% rownames(annotation)) {
				genedescription <- paste(probeset, annotation[probeset, 
																											"genesymbol"], "-", annotation[probeset, "genename"])
			}
			else {
				genedescription <- probeset
			}
			mtext(genedescription, side = 1, cex = 0.7, adj = 0, 
						padj = 8.2)
		}
	}
	if (verbose) {
		if (!is.null(colourBy)) {
			if (class(expressionset[[colourBy]]) == "numeric") {
				print(paste("Colours were encoded by '", colourBy, 
										"' on a scale from green at ", signif(min(expressionset[[colourBy]], 
																															na.rm = TRUE), 2), " to dark-brown at ", 
										signif(max(expressionset[[colourBy]], na.rm = TRUE), 
													 2), sep = ""))
			}
			else {
				colours <- colours[!is.na(names(colours))]
				print(paste("Colours were encoded by '", colourBy, 
										"' as follows: ", paste(paste(names(colours), 
																									colours, sep = "="), collapse = ", "), sep = ""))
			}
		}
		if (!is.null(pchBy)) {
			pch <- pch[!is.na(names(pch))]
			print(paste("Symbols were encoded by '", pchBy, "' as follows: ", 
									paste(paste(names(pch), pch, sep = "="), collapse = ", "), 
									sep = ""))
		}
	}
}



shinyServer(function(input, output) {
	
	
# 	dataInput <- reactive({
# 		input$data_type
# 		print(input$data_type)
# 	
# 	})
	
	output$plotGenes <- renderPlot({
		
		input$goButton
		
		
		gene1 <- isolate(input$gene1)
		gene2 <- isolate(input$gene2)
		label <- isolate(input$label)
		age_min <- isolate(input$age[1])
		age_max <- isolate(input$age[2])
		rin_min <- isolate(input$rin_min)
		colourBy <- isolate(input$colourBy)
		pchBy <- isolate(input$pchBy)
		data_type <- isolate(input$data_type)
		if(is.null(gene1) | gene1=="")return(NULL)
		
		
		#set age max to max
		if(age_max==80)age_max<-120
		if(!gene1%in%annotation_seq[,"genesymbol"])stop(paste("Gene",gene1,"not found"))
		
		if(data_type == "RPKM"){
			stop("RPKM is not implementend yet. Send and email to lassefolkersen@gmail.com to have it.")
		}
		# else if(data_type == "log2_TMM"){
# 			expressionset <- expressionset
# 		}else{stop("!")}
		
		e<-expressionset[,expressionset[["RIN"]] >= rin_min & expressionset[["Age"]] >= age_min & expressionset[["Age"]] <= age_max]
		
		probeset1<-rownames(annotation_seq)[annotation_seq[,"genesymbol"]%in%gene1]
		if(length(probeset1)!=1){
			warning("More than 1 probeset1")
			probeset1 <- probeset1[1]
		}
		
		if(label == "gene"){
			if(!gene2%in%annotation_seq[,"genesymbol"])stop(paste("Gene2",gene2,"not found"))
			probeset2<-rownames(annotation_seq)[annotation_seq[,"genesymbol"]%in%gene2]
			if(length(probeset2)!=1){
				warning("More than 1 probeset2")
				probeset2 <- probeset2[1]
			}
			fun_plot_gene_vs_gene_20100416(e, probeset1, probeset2, annotation=annotation_seq)
			
			
		}else{
			if(pchBy == "None") pchBy <- NULL
			if(colourBy == "None") colourBy <- NULL
			fun_plot_genes_vs_clinical_20110209(e, probeset1, label, annotation=annotation_seq, doStatistics=T, colourBy=colourBy,pchBy=pchBy,ylab=data_type)
		}
	})
	
	
	
	
	
	
	output$downloadData <- downloadHandler(
		filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"data.txt",sep="_"),
		content = function(file){
			gene1 <- isolate(input$gene1)
			gene2 <- isolate(input$gene2)
			label <- isolate(input$label)
			age_min <- isolate(input$age[1])
			age_max <- isolate(input$age[2])
			rin_min <- isolate(input$rin_min)
			
			
			if(is.null(gene1) | gene1=="")return(NULL)
			#set age max to max
			if(age_max==80)age_max<-120
			if(!gene1%in%annotation_seq[,"genesymbol"])stop(paste("Gene",gene1,"not found"))
			e<-expressionset[,expressionset[["RIN"]] >= rin_min & expressionset[["Age"]] >= age_min & expressionset[["Age"]] <= age_max]
			probeset1<-rownames(annotation_seq)[annotation_seq[,"genesymbol"]%in%gene1]
			if(length(probeset1)!=1){
				warning("More than 1 probeset1")
				probeset1 <- probeset1[1]
			}
			if(label == "gene"){
				if(!gene2%in%annotation_seq[,"genesymbol"])stop(paste("Gene2",gene2,"not found"))
				probeset2<-rownames(annotation_seq)[annotation_seq[,"genesymbol"]%in%gene2]
				if(length(probeset2)!=1){
					warning("More than 1 probeset2")
					probeset2 <- probeset2[1]
				}
				probesets<-c(probeset1,probeset2)
			}else{
				probesets<-probeset1
			}
			exprs<-as.data.frame(t(exprs(e)[probesets,,drop=F]))
			colnames(exprs) <- as.character(annotation_seq[probesets,"genesymbol"])
			out<-cbind(e[[label]],exprs)
			colnames(out)<-label
			write.table(out,file,col.names=NA,sep="\t")
			# write.csv("exprs(e)[probesets,]",file)
		}
	)
	
})

