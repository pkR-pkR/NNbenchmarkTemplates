# Reading documents
document<-function(path, name = NULL, pattern = "txt"){
	num = length(path) # number of sub folders
	docs = c()
	for (i in 1:num) {
		doc = c()
		if (tolower(pattern) == "txt") {
			if (is.null(name)) {
				filenames <-list.files(path[i], paste("*.", pattern, sep=""), full.names=TRUE)
				suppressWarnings(fileRead <- lapply(filenames, readLines))
				doc <- unlist(lapply(fileRead, paste, collapse = " "))
			} else {
				filename <- paste(path[i],"\\", name,".", pattern, sep="")
				doc = suppressWarnings(paste(readLines(filename), collapse = " "))
			}
		} else if (tolower(pattern) == "csv") {
			if (is.null(name)) {
				filenames <-list.files(path[i], paste("*.", pattern, sep=""), full.names=TRUE)
				suppressWarnings(fileRead <- lapply(filenames, read.csv, quote = "", header = FALSE, row.names = NULL, stringsAsFactors = FALSE,
					colClasses= "character", strip.white=TRUE))
				nDoc = length(fileRead)
				doc = c()
				for (i in 1:nDoc) {
					nline = dim(fileRead[[i]])[1]
					Lines = c()
					for (j in 1:nline) {
						curLine = paste(c(fileRead[[i]][j, ]), collapse = " ")
						curLine = gsub("^\\s+|\\s+$", "", curLine)  # remove leading and trailing blanks
						Lines = c(Lines, curLine)
					}
					doc = c(doc, paste(Lines, collapse = " "))
				}
			} else {
				filename <- paste(path[i],"\\", name,".", pattern, sep="")
				fileRead = suppressWarnings(read.csv(filename, quote = "", header = FALSE, row.names = NULL, stringsAsFactors = FALSE, 
					colClasses= "character", strip.white=TRUE))
				nline = dim(fileRead)[1]
				doc = c()
				for (j in 1:nline) {
					curLine = paste(c(fileRead[j, ]), collapse = " ")
					curLine = gsub("^\\s+|\\s+$", "", curLine)  # remove leading and trailing blanks
					doc = c(doc, curLine)
				}
				doc = paste(doc, collapse = " ")
			}
		} else {
			filenames <-list.files(path[i], paste("*.", pattern, sep=""), full.names=TRUE)
			suppressWarnings(filescan <- lapply(filenames, scan, what = " ", quote = ""))
			doc <- unlist(lapply(filescan, paste, collapse = " "))
		}
		## Combining doc from different sub folder
		docs = c(docs, doc)
	}
	return (docs)
}

wordEmbed<-function(object, dictionary, meanVec = TRUE){
	docs = object
	nDoc = length(docs)
	nDim = dim(dictionary)[2] - 1
	wordMatList = list()
	wordvec = matrix(rep(0.0, nDoc * nDim), nrow = nDoc)
	vocab = as.character(dictionary[,1])
	for (i in 1:nDoc) {
		curDoc = tolower(docs[i])
		curWordArray = unlist(strsplit(curDoc," "))
		nWord = length(curWordArray)
		wordIdx = match(curWordArray, vocab)
		idxNotFound = 1  # Set the same as first word 'a' 
		wordIdx[which(is.na(wordIdx))] = idxNotFound
		curWordMat = as.matrix(dictionary[wordIdx,2:dim(dictionary)[2]])
		rownames(curWordMat) = curWordArray
		wordMatList = c(wordMatList,list(curWordMat))
	}
	if (meanVec == TRUE) {
		for (i in 1:nDoc) {
			wordvec[i,] = colSums(wordMatList[[i]])/dim(wordMatList[[i]])[1]
		}
		colnames(wordvec) = colnames(dictionary[,-1])
		return (wordvec)
	} else {
		return (wordMatList)
	}
}

### Load URL Data
loadURLData<-function(URL, folder, unzip = FALSE){
	# Get File Name
	items = unlist(strsplit(URL,"/"))
	fileName = items[length(items)]
    if (file.exists(folder)){
        cat("folder path found", '\n')
    } else {
        # folder not existing
        cat("folder path not found, creating new directory", '\n')
        dir.create(file.path(folder), showWarnings = FALSE)
    }
	destfile = paste(folder, "\\", fileName, sep = "")
	
	download.file(URL, destfile = destfile)
	if (unzip == TRUE) {
		zipfile = destfile
		unzip(zipfile)
	}
}

### Unzip Data
# folder = "C:\\Users\\rockingdingo\\Desktop\\dev\\data"
# URL = "http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip"
# destfile = "C50.zip"






