#' @name GUImorph-package
#' @docType package
#' @aliases GUImorph
#' @title Analytical Tools for morphological Data
#' @author Erik Otarola-Castillo
#'
#' @description GUI to R programs to digitize in 3D, conduct geometric morphomteric analyses and plotting results based on OpenGL and Tk widget
NULL

#' @import geomorph
NULL

#' @import Morpho
NULL

#' @import parallel
NULL

#' @import Rvcg
NULL

#' @import tcltk
NULL

#' @import tcltk2
NULL

#' @import vegan
NULL


ui <- function(e) {
    UseMethod("ui", e)
}

init <- function(e) {
    UseMethod("init", e)
}

bind <- function(e) {
    UseMethod("bind", e)
}

updateWidgets <- function(e) {
    UseMethod("updateWidgets", e)
}

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

read.vertex.3D <- function(content, key) {
    startLines <- grep(key, content, TRUE)
    numbers <- sub(key, "", content[startLines], TRUE)

    vertexs <- array(NA, dim = c(numbers[1], 3, length(startLines)))
    for(i in 1:length(startLines)){
        startNum <- as.numeric(startLines[i])
        endNum <- startNum + as.numeric(numbers[i])
        tmp <- content[(startNum + 1):endNum]
        vertexs[,,i] <- matrix(as.numeric(unlist(strsplit(tmp, " "))), ncol=3, byrow=TRUE)
    }
    return(vertexs)
}

write.vertex.3D <- function(content, key, fileName) {
    leadLine <- paste(key, dim(content)[1], sep="")  
	write(leadLine, fileName, append = TRUE)        
	
    write.table(content, fileName, col.names=FALSE, row.names=FALSE, sep=" ", append=TRUE)
}

init.main <- function(e) {
    e$activeDataList <- list()
    e$currImgId <- 1
    e$tab <- 0
    init.digitize(e)
    init.surface(e)
    init.curve(e)
    class(e) <- "digitize"
}

switchTab <- function(e, id) {    
    e$currImgId <- 1
    
    if (id == 0) {
        e$tab <- 0
        set("window", "mode", "digitize")
        class(e) <- "digitize"
    } else if(id == 1) {
        e$tab <- 1
        set("window", "mode", "surface")
        class(e) <- "surface"
        showPicture(e)
    } else if (id == 2) {
        e$tab <- 2
        set("window", "mode", "curve")
        class(e) <- "curve"
        showPicture(e)
    } else if (id == 3) {
        e$tab <- 3
        set("window", "mode", "geomorph")
        class(e) <- "geomorph"
        showPicture(e)
    }
    bind(e)
}

#build main window
ui.main <-function(e) {
    e$wnd <- tktoplevel(width=1400, height=1200)
    tktitle(e$wnd) <- "3D GUImorph" 
    
    tn <- ttknotebook(e$wnd, width=400, height=670)
    
    centerFrame <- tkframe(e$wnd, width=650, height=670)
    
    canvasFrame <- tkframe(centerFrame, width=650, height=600, background="white", takefocus=1)
    
    controlFrame <- tkframe(centerFrame, width=650)
    fitBtn <- tkbutton(controlFrame, text = "Fit",command = function() onFit(e))
    zoomInBtn <- tkbutton(controlFrame, text = "Zoom In",command = function() zoom(e, +1))
    zoomOutBtn <- tkbutton(controlFrame, text = "Zoom Out",command = function() zoom(e, -1))
    tkgrid(fitBtn, zoomInBtn, zoomOutBtn, padx = c(10), pady = 5)
    
    btnFrame <- createNavFrame(e, centerFrame)
    
    tkpack(btnFrame)
    tkpack(canvasFrame)
    e$canvasFrame <- canvasFrame
    tkpack(controlFrame)
    
    tkbind(tn, '<Button-1>', function(W, x, y) {
        id <- tclvalue(tcl(W, "identify", "tab", x, y))
        switchTab(e, id)
    })  
    
    digitizeFrame <- ui.digitize(e, tn)    
    surfaceFrame <- ui.surface(e, tn)
    gpagenFrame <- ui.geomorph(e, tn)
    curveFrame <- ui.curve(e, tn)    

    tkadd(tn,digitizeFrame,text="3D Digitizing")
    tkadd(tn,surfaceFrame,text="Surface Sliders")
    tkadd(tn,curveFrame,text="Curves")
    tkadd(tn,gpagenFrame,text="GPA")
    
    #tkpack(tn)
    sapply(list(centerFrame, tn), tkpack, side = "left", padx = 6)
    
    print("set window id size")
    set("window", "id", canvasFrame)
    set("window", "size", 600, 600)
    
    createMenu(e)
    bind.digitize(e)
}

createMenu <- function(e) {
    topMenu <- tkmenu(e$wnd)
    tkconfigure(e$wnd, menu = topMenu) 
    
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Load ply File",command = function() loadPly(e))
    tkadd(fileMenu, "command", label = "Save to DGT",command = function() saveToDgt(e))
    tkadd(fileMenu, "command", label = "Load DGT File",command = function() openDgt(e))
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(e$wnd))
    tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
}

createNavFrame <- function(e, parent) {
    btnFrame <- ttkframe(parent)    
    prevBtn <- ttkbutton(btnFrame, text = "< Previous", command = function() onPrevious(e))    
    nextBtn <- ttkbutton(btnFrame, text = "Next >",command = function() onNext(e))
    e$imgPath <- ttklabel(btnFrame, text = "Specimen Id: NA")
    
    tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "both", side = "left")
    sapply(list(prevBtn, nextBtn, e$imgPath), tkpack, side = "left", padx = 6)
    tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "both", side = "left")
    return (btnFrame)
}

showPicture <- function(e) {
    if(length(e$activeDataList) == 0) {return ()}
    
    imgId <- e$currImgId
    set("specimen", "id", e$currImgId - 1)
    zoom <- e$activeDataList[[imgId]][[7]]
    angelX <- e$activeDataList[[imgId]][[6]][1]
    angelY <- e$activeDataList[[imgId]][[6]][2]

    set("specimen", "angle", "x", angelX)
    set("specimen", "angle", "y", angelY)
    while (zoom > 0) {
       set("specimen", "scale", "in")
       zoom = zoom - 1
    }
    while (zoom < 0) {
       set("specimen", "scale", "out")
       zoom = zoom + 1
    }
    updateWidgets(e)
}

zoom <- function(e, D) {  
    if(length(e$activeDataList)) {
        imgId <- e$currImgId
        zoomValue <- e$activeDataList[[imgId]][[7]]
        if (D > 0) {
            set("specimen", "scale", "in")
            zoomValue <- zoomValue + 1
        } else {
            set("specimen", "scale", "out")
            zoomValue <- zoomValue - 1
        }
        e$activeDataList[[imgId]][[7]] <- zoomValue
    }    
}

motion <- function(e, x, y) {
    if(length(e$activeDataList) == 0) {return()}
    if (e$dragX == -1 || e$dragY == -1) {
        return()
    }
    
    x <- as.integer(x)
    y <- as.integer(y)
    
    if (e$dragDot) {
        #print("move it to:" )
        coord <- convertCoor(e, x, y)
        set("dot", "coordinate", coord[1], coord[2], coord[3])
        return()
    }

    dx <- abs(e$dragX - x)
    dy <- abs(e$dragY - y)
    if (dx < 10 && dy < 10) {
        return()
    }
    
    imgId <- e$currImgId
    preAngle <- e$activeDataList[[imgId]][[6]]
    
    if (dx > dy) {        
        angle <- (x - e$dragX) / 600  * 360
        msg <- set("specimen", "angle", "y", angle)
        currAngle <- preAngle + c(0, angle)
    } else {
        angle <- (y - e$dragY) / 600 * 360
        msg <- set("specimen", "angle", "x", angle)
        currAngle <- preAngle + c(angle, 0)
    }
    
    e$dragX <- x
    e$dragY <- y
    e$activeDataList[[imgId]][[6]] <- currAngle
}

#show next specimen
onNext <- function(e) {
    if(length(e$activeDataList) == 0) {return ()}

    if(e$currImgId == length(e$activeDataList)) {
        tkmessageBox(title = "Information", message = "It's the last specimen", icon = "info", type = "ok")
        return ()
    }
    
    if(e$tab == 0) {
        nCurrLM <- e$activeDataList[[e$currImgId]][[3]]
        if(nCurrLM < as.integer(e$landmarkNum)) {
            tkmessageBox(title = "Information", message = "Incorrect number of landmarks", icon = "info", type = "ok")            
            return ()
        } 
    }
    
    e$currImgId <- e$currImgId+1
    showPicture(e)
}

#show the previous specimen
onPrevious <- function(e) {
     if(length(e$activeDataList) == 0) {return ()}
    if(e$currImgId == 1) {
        tkmessageBox(title = "Information", message = "It's the first specimen", icon = "info", type = "ok")
        return ()
    }
    
    if(e$tab == 0) {
        nCurrLM <- e$activeDataList[[e$currImgId]][[3]]
        if(nCurrLM < as.integer(e$landmarkNum)) {
            tkmessageBox(title = "Information", message = "Incorrect number of landmarks", icon = "info", type = "ok")
        }    
    }
    
    e$currImgId <- e$currImgId -1
    showPicture(e)
}

onFit <- function(e) {
    if(length(e$activeDataList) == 0) { return() }
    
    imgId <- e$currImgId
    zoom <- e$activeDataList[[imgId]][[7]]
    angelX <- e$activeDataList[[imgId]][[6]][1]
    angelY <- e$activeDataList[[imgId]][[6]][2]
    
    set("specimen", "angle", "x", -angelX)
    set("specimen", "angle", "y", -angelY)
    
    while (zoom > 0) {
       set("specimen", "scale", "out")
       zoom = zoom - 1
    }
    
    while (zoom < 0) {
       set("specimen", "scale", "in")
       zoom = zoom + 1
    }
    
    e$activeDataList[[imgId]][[6]] <- c(0, 0)
    e$activeDataList[[imgId]][[7]] <- 0
}

popUpRemoveWindow <- function(e, x, y, msg, item) {
    win <- tktoplevel()

    label = tklabel(win, text=msg)
    tkpack(label, fill = "x", padx = 5, pady = 5)

    btnFrame <- ttkframe(win)
    tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
    
    if(item == "digdot") {
        okBtn <- ttkbutton(btnFrame, text = "ok",command = function() digRemoveDotOk(e, x, y))
        cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() digRemoveDotCancel(e, x, y))
    }else if(item == "curve") {
        #okBtn <- ttkbutton(btnFrame, text = "ok",command = function() linkRemoveLineOk(e, x, y))
        #cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
    }

    tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
    sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

    e$removeWin <- win
    tkfocus(win)
}


# get .ply file
loadPly <- function(e) {
  fileStr <- tclvalue(tkgetOpenFile( filetypes = "{{ply file} {.ply}}", multiple=TRUE, title="Select Images to Digitize"))

  if (length(grep(pattern = "}",x = fileStr)) >0 ){
    imgList <- unlist(strsplit(fileStr, "} ",fixed = FALSE))
    imgList <- gsub(pattern = "}",replacement = "",x = imgList)
    imgList <- gsub(pattern = "\\{",replacement = "",x = imgList)
  } else {
    imgList <- unlist(strsplit(fileStr, " ",fixed = FALSE))
  }
  
    nSpecimens <- length(imgList)

    if (nSpecimens != 0) {
        #initialize dgtDataList
        dgtDataList <- list()
        for(i in 1:length(imgList)){
            speciName <- imgList[[i]]
            if(!file.exists(speciName)) {
                nSpecimens <- nSpecimens-1
                print(paste(speciName, "doesn't exist. Ignore it!!"))
                next
            }

            dgtDataList[[length(dgtDataList)+1]] <- list(imgList[[i]], 0.01, 0, list(), "NULL", c(0, 0), 0, "NULL")
        }

        if(nSpecimens > 0) {
            #initialize
            init.digitize(e)

            e$activeDataList <- dgtDataList
            e$digData <- dgtDataList
            e$currImgId <- 1
            
            tkconfigure(e$specimenNumLabel, text = paste("Number of Specimens: ", nSpecimens))
            tkconfigure(e$imgPath, text = paste("Specimen Id: ", dgtDataList[[1]][[1]]))    

            set("specimen", "amount", length(dgtDataList))
            for(i in 1:length(dgtDataList)) {
                add("specimen", dgtDataList[[i]][[1]], i-1) #adds it to the canvas (first loads model, then uses onDisplay() in C to render)
            }
        }
    }
}

openDgt <- function(e) {
    dgtfileName <- tclvalue(tkgetOpenFile( filetypes = "{DGT {.dgt}}"))

    if (dgtfileName != "") {        
		rawContent <- scan(file = dgtfileName, what = "char", sep = "\n", quiet = TRUE)
		
		################### read digitize data ##################
		olddat <- read.digitize(rawContent)		
        nSpecimens <- dim(olddat)[3]
		
		################### read template ##################
		templOrig <- read.template(rawContent)
		print(paste("e$templOrig", e$templOrig))
		
		################### read surface data ##################
		surfaceData <- read.surface(rawContent)
		tmpt <- surfaceData$template
        surfaces <- surfaceData$surfaces
		
		if((length(tmpt) != nSpecimens) | (dim(surfaces)[3] != nSpecimens)) {
			tkmessageBox(title = "Error", message = "Incorrect format of dgt file", icon = "info", type = "ok")
			return ()
		}

		################### read curves data ##################
		curves <- read.curve(rawContent)
		
		################### Add elements ##################
        if(nSpecimens > 0) {
            init.main(e)
			e$templOrig <- templOrig
            
			drawElements(e, olddat, surfaceData, curves)
            loadDgt(dgtfileName)
			
            if(e$tab == 0) {  
				string <- "digitize"               
            }else if (e$tab == 1) {
				string <- "surface"
            }else if(e$tab == 2) {
				string <- "curve"
            }
			class(e) <- string    
            set("window", "mode", string)
            
            updateWidgets(e)
            print("Load gts file completed")
        }
    }
}

drawElements <- function(e, digitize, surfaceData, curves) { 
	print("drawElements")
	specimens <- dimnames(digitize)[[3]]
	nSpecimens <- dim(digitize)[3]
	e$landmarkNum <- dim(digitize)[1]
	tmpt <- surfaceData$template
    surfaces <- surfaceData$surfaces
    
	dgtDataList <- list()
	specId <- 1
	
	set("window", "mode", "none")
	set("specimen", "amount", nSpecimens)
	for(i in 1:nSpecimens){              
		if(!file.exists(specimens[[i]])) {
			nSpecimens <- nSpecimens-1
			print(paste(specimens[[i]], "doesn't exist. Ignore it!!"))
			next
		}    

		landmarks <- digitize[, , i]		                              
		draw.digitize(i, specimens[[i]], landmarks)
		
		dgtDataList[[specId]] <- list(specimens[[i]], 0.01, e$landmarkNum, list(), tmpt[i], c(0, 0), 0, surfaces[,,i], landmarks)
		specId <- specId+1
	}
	
	if (length(curves) != 0) {
		draw.curves(curves)
		dgtDataList[[1]][[4]] <- curves
	}
	e$activeDataList <- dgtDataList
	set("specimen", "id", 0)
}

saveToDgt <- function(e) {
    nSpecimen <- length(e$activeDataList)
    if(nSpecimen <= 0) {
        tkmessageBox(title = "Information", message = "Nothing to be saved", icon = "info", type = "ok")
        return ()
    }
    
    #select the location
    fileName <- tclvalue(tkgetSaveFile(filetypes="{DGT {.dgt}}"))
    if (!nchar(fileName)) {
        return ()
    }
	
    if (length(grep(".dgt",x = fileName)) == 0 ) {
      fileName <- paste(fileName,".dgt", sep = "")
    } 
    
	file.create(fileName, showWarnings=TRUE)
	 
	################### write curve #####################
	curves <- e$activeDataList[[1]][[4]]
	print(paste("curves", curves))
	write.curve(fileName, curves)
	
	################### write template ####################
	write.template(fileName, e$templOrig)
	
    for(i in 1:nSpecimen){
	
		################### write landmark #####################
        specimenId <- e$activeDataList[[i]][[1]]
		landmarks <- getLandmark(i)
		 if(is.null(landmarks)) {
            tkmessageBox(title = "Information", message = "No landmark. Nothing to be saved", icon = "info", type = "ok")
            next  
        } 
		write.digitize(fileName, specimenId, landmarks)

		################### write surface #####################		
        tempt <- e$activeDataList[[i]][[5]]
        surface <- e$activeDataList[[i]][[8]] 
		write.surface(fileName, tempt, surface)		
		
		write("", fileName, append = TRUE)
    }
}

getLandmark <- function(id) {
    lmkStr <- tclvalue(shows("landmark", "xyz", id-1))
    if(lmkStr != "") {
        lmkV <- strsplit(lmkStr, " ")[[1]]
        rows <- length(lmkV)/3
        lmk = matrix(as.numeric(lmkV), nrow = rows, ncol = 3, byrow = TRUE)
    }else {
        lmk <- NULL
    }

    return(lmk)
}

convertCoor <- function(e, x, y) {
    dotStr <- shows("specimen", "xyz", x, y)
    dot <- strsplit(tclvalue(dotStr), " ")
    realDot <- c(as.numeric(dot[[1]][1]), as.numeric(dot[[1]][2]), as.numeric(dot[[1]][3]))
    #print(paste("convert ", x, y, "to ",realDot[1], realDot[2], realDot[3]))
    return (realDot)
}