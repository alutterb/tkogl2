################# main data structure ##############################
#dgtDataList
#dgtDataList[imgId][[1]]: speciman dir
#dgtDataList[imgId][[2]]: font
#dgtDataList[imgId][[3]]: number of landmark
#dgtDataList[1][[4]]: curves
#dgtDataList[imgId][[5]]: template
#dgtDataList[imgId][[6]]: rotation
#dgtDataList[imgId][[7]]: zoom
#dgtDataList[imgId][[8]]: surface file

#initializes parameter for surface component
init.surface <- function(e) {
	#print("surfaceInit")
	e$dragX <- as.integer(-1)
	e$dragY <- as.integer(-1)
	e$sliderNum <- 1000
	e$surOnlyMode <- FALSE
	e$templOrig <- "NULL"
	e$aDimR <- 0
	e$aDimC <- 0
}

#draw other widgets
ui.surface <- function(e, parent) {
	#print("ui.surface ")
	surCtlFrame <- ttkframe(parent)
	e$bt1 <- NULL

	fitBtn <- ttkbutton(surCtlFrame, text = "Fit",command = function() onFit(e))
	tkpack(ttklabel(surCtlFrame, text = " "), pady = 6)
	tkpack(fitBtn)
	
	setSliderNumBtn <- ttkbutton(surCtlFrame, text = "Set number of surface sliders", command = function() setSurSliderNum(e))
	tkpack(setSliderNumBtn)
	
	e$useAnchorVar <- tclVar("0")
	useAnchor <- ttkcheckbutton(surCtlFrame, text = "Use Anchors for Downsampling", variable = e$useAnchorVar, command = function(){})
	assign("bt1", useAnchor, envir = e)
	tkpack(useAnchor)
	
	entryFrame1 <- ttkframe(surCtlFrame)
	tmplLabel = tklabel(entryFrame1, text='Current Template: ')
	e$tmplVar <- tclVar("NA")
	e$tmplEntry = tkentry(entryFrame1, textvariable=e$tmplVar, state="readonly", width=35)
	sapply(list(tmplLabel, e$tmplEntry), tkpack, pady = 3)
	
    buldTemBtn <- ttkbutton(surCtlFrame, text = "Build Template",command = function() buildTemplate(e))
	e$buldTemBtn <- buldTemBtn
	
	tkpack(ttklabel(surCtlFrame, text = " "), pady = 5)
	sapply(list(entryFrame1, buldTemBtn), tkpack, pady = 3)
	
	entryFrame2 <- ttkframe(surCtlFrame)
	downsmplLabel = tklabel(entryFrame2, text='Current downsample: ')
	e$downsmplVar <- tclVar("NA")
	e$downsmplEntry = tkentry(entryFrame2, textvariable=e$downsmplVar, state="readonly", width=35)
	sapply(list(downsmplLabel, e$downsmplEntry), tkpack, pady = 3)
	
	e$downSampleBtn <- ttkbutton(surCtlFrame, text = "Downsample specimen",command = function() downSample(e))

	tkpack(ttklabel(surCtlFrame, text = " "), pady = 5)
	sapply(list(entryFrame2, e$downSampleBtn), tkpack, pady = 3)
	
	e$switchBtn <- ttkbutton(surCtlFrame, text = "Toggle specimen",command = function() hidePly(e))
	tkpack(ttklabel(surCtlFrame, text = " "), pady = 5)
	tkpack(e$switchBtn)
	
	return (surCtlFrame)
}

#configures button press for surface component
bind.surface <-function(e) {   
	tkbind(e$canvasFrame, "<ButtonPress-1>", function(x, y) {
		if(length(e$activeDataList) > 0) {	
			e$dragX <- as.integer(x)
			e$dragY <- as.integer(y)
		}
	})
	
	tkbind(e$canvasFrame, "<ButtonRelease-1>", function(x, y) {
		if(length(e$activeDataList) > 0) {
			e$dragX <- as.integer(-1)
			e$dragY <- as.integer(-1)
		}	
	})
	
	#tkbind(e$canvasFrame, "<Motion>", function(x, y) {motion.surface(e, x, y)})
	
	tkbind(e$canvasFrame, "<ButtonPress-3>", function(x, y) { })	
	tkbind(e$canvasFrame, "<Double-Button-1>", function(x, y) {	})
}


#User interface layout dynamic update callback
updateWidgets.surface <- function(e) {
	#print("updateWidgets.surface")

	e$tmplVar <- tclVar(paste("Based on ", e$templOrig))
	tkconfigure(e$tmplEntry , textvariable=e$tmplVar)
	
	templ <- e$activeDataList[[e$currImgId]][[5]]
	if(templ == "NULL") {
		e$downsmplVar <- tclVar("NA")
	}else {
		e$downsmplVar <- tclVar(paste("based on ",  templ))
	}
	tkconfigure(e$downsmplEntry , textvariable=e$downsmplVar)
}

#hides or shows specimen on canvas
hidePly <- function(e) {
	#print("hidePly")
	if(e$surOnlyMode) {
		set("window", "mode", "surface")
		e$surOnlyMode <- FALSE
	}else {
		set("window", "mode", "surfaceonly")
		e$surOnlyMode <- TRUE
	}
}

#Pop up window to configure number of surface sliders
setSurSliderNum <- function(e) {
	win <- tktoplevel()
	tkwm.title(win, "Set Number of Surface Sliders")

	entryFrame <- ttkframe(win)
	tkpack(entryFrame, expand = TRUE, fill = "both", padx = 5, pady = 5)
	label = tklabel(entryFrame, text='Set Number of Surface Sliders: ')

    e$sliderEntry = tkentry(entryFrame, textvariable=tclVar(e$sliderNum))
	sapply(list(label, e$sliderEntry), tkpack, side = "left", padx = 6)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	okBtn <- ttkbutton(btnFrame, text = "ok",command = function() onSliderNumOk(e, win))

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	tkfocus(win)
}

#Initiates to set the number of surface sliders
onSliderNumOk <- function(e, win) {
    e$sliderNum <- tclvalue(tkget(e$sliderEntry))
	tkdestroy(win)
}

#Enables or disables button for build template and down sample
disableOper<-function(e, state) {
	if (state) {
		tkconfigure(e$buldTemBtn, state="disabled")
		tkconfigure(e$downSampleBtn, state="disabled")
	} else {
		tkconfigure(e$buldTemBtn, state="enabled")
		tkconfigure(e$downSampleBtn, state="enabled")
	}
}

#builds template according to current specimen
buildTemplate <- function(e) {
	disableOper(e, T)
	lmk <- getLandmark(e$currImgId)
	if(is.null(lmk)) {
	  print("No landmarks. Downsampling is not allowed.")
	  return ()
	}
	#check anchor toggle and if anchors are present
	anc <- getAnchor(e$currImgId)
	if(is.null(anc)) {
	    print("No anchors. Cannot use in downsampling process.")
	    if(tclvalue(e$useAnchorVar) == "1")
	        return()
	}
	
	e$aDimR <- dim(anc)[1]
	e$aDimC <- dim(anc)[2]
	
	fileName <- e$activeDataList[[e$currImgId]][[1]]
	
	
	#### 12.14.2017 changed read ply function from geomorph's read.ply to Rvcg's vcgPlyRead
	# much faster
	spec <- Rvcg::vcgPlyRead(fileName, updateNormals = TRUE, clean = FALSE)
	#spec <- read.ply(fileName, ShowSpecimen = FALSE)

	#surface.sliders<-1000
	specimen <- as.matrix(t(spec$vb)[, -4])
	
	#### 12.14.2017 changed kmeans function from Rs base kmeans to Morpho's fastKmeans
	# much faster
	#if no anchors
	if(tclvalue(e$useAnchorVar) == "0")
        template <- rbind(lmk,Morpho::fastKmeans(x=specimen,k=as.numeric(e$sliderNum),iter.max=100,project=TRUE)$centers)
	if(tclvalue(e$useAnchorVar) == "1") {
	    lmkAnc <- rbind(lmk, anc)
	    template <- rbind(lmkAnc,Morpho::fastKmeans(x=specimen,k=as.numeric(e$sliderNum),iter.max=100,project=TRUE)$centers)
	}
	# template <- rbind(lmk,kmeans(x=specimen,centers=e$sliderNum,iter.max=100)$centers)
	write.table(template,file="template.txt",row.names=F,col.names=TRUE)
	disableOper(e, F)
	
	e$tmplVar <- tclVar(paste("Based on ", basename(fileName)))
	tkconfigure(e$tmplEntry , textvariable=e$tmplVar)
			
	e$templOrig <- basename(fileName)
	tkmessageBox(title = "Information", message = "Template created", icon = "info", type = "ok")
}

#
buildTemplate1 <- function(e) {
	#print("buildTemplate")
	fileName <- e$activeDataList[[e$currImgId]][[1]]
	
	#### 12.14.2017 changed read ply function from geomorph's read.ply to Rvcg's vcgPlyRead
	# much faster
	spec <- Rvcg::vcgPlyRead(fileName, updateNormals = TRUE, clean = FALSE)
	#spec <- read.ply(fileName, ShowSpecimen = FALSE)

	spec.name<-deparse(substitute(spec))
	mesh <- NULL
	if (inherits(spec, "shape3d") == TRUE || inherits(spec, "mesh3d") == TRUE){
		specimen <- scale(as.matrix(t(spec$vb)[,-4]), scale = FALSE)
		spec$vb <- rbind(t(specimen), 1)
		mesh <- spec 
		if (is.null(mesh$material)) { mesh$material <- "gray" } 
	} else if (inherits(spec, "matrix") == FALSE) {
		stop ("File is not a shape3d/mesh3d object or xyz matrix")
	} else if (inherits(spec, "matrix") == TRUE && dim(spec)[2]==3) {
		pecimen <- scale(spec, scale = FALSE)		
	} else { stop ("File is not matrix in form: vertices by xyz")} 
	
	lmkStr <- tclvalue(shows("specimen", "landmark"))
	lmkV <- strsplit(lmkStr, " ")[[1]]

	rows <- length(lmkV)/3
	lmk = matrix( as.numeric(lmkV), nrow = rows, ncol = 3, byrow = TRUE)
	#sliders <- 1000
	
	template <- rbind(lmk,Morpho::fastKmeans(x=specimen,k=as.numeric(e$sliderNum),iter.max=100,project=TRUE)$centers)
	#template <- rbind(lmk,kmeans(x=specimen,centers=e$sliderNum,iter.max=100)$centers)
	write.table(template,file="template.txt",row.names=F,col.names=TRUE)
	
	e$templOrig <- basename(fileName)
	e$tmplVar <- tclVar(paste("Based on ", e$templOrig))
	tkconfigure(e$tmplEntry , textvariable=e$tmplVar)
}

#write sample data to file
write.nts <- function(vertexNum, fileName, vertex) {
	file.create(fileName, showWarnings=TRUE)
	write(paste("Surface=", vertexNum, sep=""), file = fileName)
	write.table(vertex, file=fileName, row.names=F, col.names=F, append = TRUE)
}

#Performs downsample for the current specimen
downSample <- function(e) {
	disableOper(e, T)
  lmk <- getLandmark(e$currImgId)
  if(is.null(lmk)) {
    print("No landmarks. Downsampling is not allowed.")
    return ()
  }
  anc <- getAnchor(e$currImgId)
 
	fixed <- as.integer(dim(lmk)[1]) 
	aFixed <- as.integer(dim(anc)[1])
	center <- FALSE
	fileName <- e$activeDataList[[e$currImgId]][[1]]	
	#### 12.14.2017 changed read ply function from geomorph's read.ply to Rvcg's vcgPlyRead
	# much faster
	spec <- Rvcg::vcgPlyRead(fileName, updateNormals = TRUE, clean = FALSE)
	#spec <- read.ply(fileName, ShowSpecimen = FALSE)

	if(length(fixed)==1 && fixed<4){stop ("Number of fixed points is not sufficient.")}
	#if(length(aFixed)==1 && aFixed<4){stop ("Number of anchors is not sufficient.")}
  spec.name<-deparse(substitute(spec))
  mesh <- NULL
  if (inherits(spec, "shape3d") == TRUE || inherits(spec, "mesh3d") == TRUE){
    if (center == TRUE){
      specimen <- scale(as.matrix(t(spec$vb)[,-4]), scale = FALSE)
      spec$vb <- rbind(t(specimen), 1)
    }
    if (center == FALSE){ specimen <- as.matrix(t(spec$vb)[,-4]) }
    mesh <- spec 
    if (is.null(mesh$material)) { mesh$material <- "gray" }
  } else if (inherits(spec, "matrix") == FALSE) {
    stop ("File is not a shape3d/mesh3d object or xyz matrix")
  } else if (inherits(spec, "matrix") == TRUE && dim(spec)[2]==3) {
    if (center == TRUE){ specimen <- scale(spec, scale = FALSE) }
    if (center == FALSE){ specimen <- spec }
  } else { stop ("File is not matrix in form: vertices by xyz")} 
  
  if(tclvalue(e$useAnchorVar) == 1 && is.null(anc))
  {
      print("No anchors placed, cannot use in downsampling process")
      return()
  }
  else if(tclvalue(e$useAnchorVar) == 1)
  {
      lmk <- rbind(lmk,anc)
      fixed <- fixed + aFixed
  }
  
  template<-as.matrix(read.table("template.txt",header=TRUE))
  #specimen<-center(as.matrix(specimen))
  specimen<-as.matrix(specimen)
  #warping process
  template<-template*(cSize(lmk)/cSize(template[(1:fixed),]))
  template<-template%*%rotate.mat(lmk,template[(1:fixed),]) ###
  cat("\nWarping template\n")
  template.tps<-tps2d3d(template[-(1:fixed),],template[(1:fixed),],lmk)             
  spec.surfs<-specimen
  nei<-numeric(dim(template.tps)[1])
  sliders<-matrix(NA,nrow=dim(template.tps)[1],ncol=3)
  #now apply warping to semilandmarks
  for (i in 1:dim(template.tps)[1])     {
      nei[i]<-which.min(sqrt((template.tps[i,1]-spec.surfs[,1])^2+(template.tps[i,2]-spec.surfs[,2])^2+(template.tps[i,3]-spec.surfs[,3])^2))[1] #3D NN
      sliders[i,]<-spec.surfs[nei[i],]
      spec.surfs<-spec.surfs[-nei[i],]  
  }
  selected.out <- rbind(lmk,sliders)
  ntsFile <- paste(fileName, ".nts", sep="")
  write.nts(as.numeric(e$sliderNum), ntsFile, selected.out) 
  print("write to file")
  #write.table(selected.out, file=ntsFile, row.names=F, col.names=F, append = TRUE)
  print("add down sample")
  add("downsample", ntsFile, e$currImgId - 1)
  file.remove(ntsFile)
  disableOper(e, F)
  
  e$activeDataList[[e$currImgId]][[5]] <- e$templOrig
  e$activeDataList[[e$currImgId]][[8]] <- sliders
 
  
  tkmessageBox(title = "Information", message = "Specimen is downsampled", icon = "info", type = "ok")
  
  print(paste("e$templOrig is ", e$templOrig))
  e$downsmplVar <- tclVar(paste("based on ",  e$templOrig))
  tkconfigure(e$downsmplEntry , textvariable=e$downsmplVar)
}


#loads surface data from .dgt file
read.surface <- function(content) {
	print("read.surface")
	ignore.case = TRUE
    tmpt <- sub("Template=", "", content[grep("Template=", content, ignore.case)], ignore.case)
    surfaces <- read.vertex.3D(content, "Surface=")    
    return(list(template=tmpt, surfaces=surfaces))
}

#writes surface data to .dgt file
write.surface <- function(fileName, tempt, surface) {
	print("write.surface")
	#write template
	if(!is.null(tempt)) {
		temptLine <- paste("Template=", tempt, sep="")  
		write(temptLine, fileName, append = TRUE)            
	}

	#write downsample data
	if(length(surface) != 0) {
        write.vertex.3D(surface, "Surface=", fileName)
	}else {  
		write("Surface=NULL", fileName, append = TRUE)    
	}
}

#writes template data to .dgt file
write.template <- function(fileName, templOrig) {
	print("write.template")
	temptLine <- "TemplateNumber=NULL"
	if(file.exists("template.txt")) {
		tgtfile <- scan(file = "template.txt", what = "char", sep = "\n", quiet = TRUE)
		temptLine <- paste("TemplateNumber=", length(tgtfile), sep="") 
		write(temptLine, fileName, append = TRUE) 		
		write(templOrig, fileName, append = TRUE)
		write(tgtfile, fileName, append = TRUE)
	}else {
		write(temptLine, fileName, append = TRUE) 	
	}	
	write("",fileName,append = TRUE)
}

#reads template data from .dgt file
read.template <- function(rawContent) {
	print("read.template")
	print(rawContent[3])
	ignore.case = TRUE
	
	startLines <- grep("TemplateNumber=", rawContent, ignore.case)
	if (length(startLines) == 0) {
		return()
	}
    len <- sub("TemplateNumber=", "", rawContent[grep("TemplateNumber=", rawContent, ignore.case)], ignore.case)
	basedLine <- as.numeric(startLines[1]) + 1
	dataStart <- as.numeric(startLines[1]) + 2
	if (len[1] != "NULL") {
		print("not null")
		fileName <- "template.txt"
		file.create(fileName, showWarnings=TRUE)
		endLine <- basedLine + as.numeric(len[1])
		content <- rawContent[dataStart:endLine]
		print(paste("startline:", startLines[1], "basedLine: ", basedLine, "dataStart: ", dataStart, "endline: ", endLine))
		write(content, fileName, append = TRUE)
		print("based")
		print(rawContent[basedLine])
		return (rawContent[basedLine])
	}
	return (NULL)
}

#shows sample data on canvas
draw.surface <- function(id, surface) {
	# print(paste("Add surface for  specimen", id, "... ..."))
	# print(surface)
	# add("downsample", surface, id - 1)
}

test <- function() {
    template<-as.matrix(read.table("template.txt",header=TRUE))
    #specimen<-center(as.matrix(specimen))
    specimen<-as.matrix(specimen)
    #warping process
    template<-template*(cSize(lmk)/cSize(template[(1:fixed),]))  
    template<-template%*%rotate.mat(lmk,template[(1:fixed),])
    cat("\nWarping template\n")
    template.tps<-tps2d3d(template[-(1:fixed),],template[(1:fixed),],lmk)             
    spec.surfs<-specimen
    nei<-numeric(dim(template.tps)[1])
    sliders<-matrix(NA,nrow=dim(template.tps)[1],ncol=3)
    #now apply warping to semilandmarks
    for (i in 1:dim(template.tps)[1])     {
        nei[i]<-which.min(sqrt((template.tps[i,1]-spec.surfs[,1])^2+(template.tps[i,2]-spec.surfs[,2])^2+(template.tps[i,3]-spec.surfs[,3])^2))[1] #3D NN
        sliders[i,]<-spec.surfs[nei[i],]
        spec.surfs<-spec.surfs[-nei[i],]  
    }
    selected.out <- rbind(lmk,sliders)
    ntsFile <- paste(fileName, ".nts", sep="")
    write.nts(as.numeric(e$sliderNum), ntsFile, selected.out) 
    print("write to file")
    #write.table(selected.out, file=ntsFile, row.names=F, col.names=F, append = TRUE)
    print("add down sample")
    add("downsample", ntsFile, e$currImgId - 1)
    file.remove(ntsFile)
    disableOper(e, F)
    
    e$activeDataList[[e$currImgId]][[5]] <- e$templOrig
    e$activeDataList[[e$currImgId]][[8]] <- sliders
}