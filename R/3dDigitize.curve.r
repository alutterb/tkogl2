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

#initializes parameters for curve component
init.curve <- function(e) {
	e$curveDotNum <- 0
	e$curveDots <- c()
	e$curveLine<- c()
	e$sliders<-c()
}

#creates user interface layout for curve component
ui.curve <- function(e, parent) {
	curveCtlFrame <- ttkframe(parent)
	
	fitBtn <- ttkbutton(curveCtlFrame, text = "Fit",command = function() onFit(e))
	tkpack(ttklabel(curveCtlFrame, text = " "), pady = 6)
	tkpack(fitBtn)
	
	return (curveCtlFrame)
}

#drag and place landmarks on curve component
bind.curve <-function(e) {   
	#print("bind.curve")
	tkbind(e$canvasFrame, "<ButtonPress-1>", function(x, y) {
		e$dragX <- as.integer(x)
		e$dragY <- as.integer(y)
	})

	tkbind(e$canvasFrame, "<ButtonPress-3>", function(x, y) {})
	tkbind(e$canvasFrame, "<Double-Button-1>", function(x, y) {onSelectCurve(e, x, y)})
}

#loads curve data from .dgt file
read.curve <- function(content) {
	#print("read.curve")
	ignore.case = TRUE
	startLine <- grep("Curve=", content, ignore.case)
	num <- sub("Curve=", "", content[startLine], ignore.case)
	
	if (num == 0) {
		return (NULL)
	}
	endLine <- as.numeric(startLine) + as.numeric(num)
	startLine <- startLine + 1
	tmp <- content[startLine:endLine]
	curves <- matrix(as.numeric(unlist(strsplit(tmp, " "))), ncol=3, byrow=TRUE)		
	
	return (curves)	
}

#writes the curve data to .dgt file
write.curve <- function(fileName, curves) {
    if(length(curves) > 0) {
        write(paste("Curve=", nrow(curves),sep=""), fileName, append = TRUE)  
    } else {
        write(paste("Curve=0",sep=""), fileName, append = TRUE)
    }
    if(length(curves) > 0) {        
        write.table(curves, fileName, sep = " ", col.names = FALSE, row.names = FALSE,append=TRUE)
        write("",fileName,append = TRUE)
    } 
}

#display curves to GUI
draw.curves <- function(curves) {
	print("Add curves ... ...")
	for (j in 1:nrow(curves)) {
		add("curve", curves[j,1], curves[j,2], curves[j,3])
	}	
}	

#UI layout dynamic update callback
updateWidgets.curve <- function(e) {
	
}

#changes rgb values of selected dot to desired color
changeDotColor<-function(e) {
	print("changeDotColor")
    for(i in 1:3){
		x <- e$curveDots[[(i - 1) * 3 + 1]]
		y <- e$curveDots[[(i - 1) * 3 + 2]]
		id <- e$curveDots[[(i - 1) * 3 + 3]]

		if (set("dot", "selected", x, y)) {
			if (id %in% e$sliders) {
				set("dot", "color", 0.0, 0.0, 1.0)
			} else {
				set("dot", "color", -1.0, -1.0, -1.0)
			}
		}
	}
}

#sets and configures dot on curve
onSelectCurve <- function(e, x, y) {
	#print("onSelectCurve")
	
	if (set("dot", "selected", x, y)) {
		id <- tclvalue(shows("landmark", "id"))
		if (id %in% e$curveLine) {
			tkmessageBox(title = "Information", message = "Duplicate dot in one curve is not allowed", icon = "info", type = "ok")
			return ()
		}
		e$curveLine <- c(e$curveLine, as.numeric(id))
		
		e$curveDots <- c(e$curveDots, c(x, y, id))
		e$curveDotNum <- e$curveDotNum + 1
		
		set("dot", "color", as.double(1/255), as.double(164/255), as.double(191/255))
		
		if (e$curveDotNum == 2) {
			e$sliders<-c(e$sliders, id)
			print(e$sliders)
		} else if (e$curveDotNum == 3) {	
			set("window", "mode", "digitize")
			changeDotColor(e)
			set("window", "mode", "curve")
			curves <- e$activeDataList[[1]][[4]]
			newCurve<-matrix(e$curveLine,nrow=1,ncol=3)
			curves <- rbind(curves, newCurve)
			e$activeDataList[[1]][[4]] <- curves
			add("curve", e$curveLine[1], e$curveLine[2], e$curveLine[3])
			
			e$curveDots <- c()
			e$curveDotNum <- 0
			e$curveLine <- c()
		}
	}
}