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

init.geomorph <- function(e) {

}

#configures gui and initializes values
ui.geomorph <- function(e, parent) {
    gpagenCtlFrame <- ttkframe(parent)
    tkgrid(ttklabel(gpagenCtlFrame, text = " "), pady = 6)
    
    fitBtn <- ttkbutton(gpagenCtlFrame, text = "Fit",command = function() onFit(e))
    tkgrid(fitBtn)

    e$maxiter <- tclVar(2)
    tkgrid(tk2label(gpagenCtlFrame, text = "Maximum GPA iterations"), sticky = "w", padx=20)
    tkgrid(tk2entry(gpagenCtlFrame, textvariable = e$maxiter, width = "25"), sticky = "w", padx=20)
    e$anchorsSurface <- tclVar(0)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Use anchors as surface semilandmarks", variable = e$anchorsSurface), sticky = "w")
    e$anchorsCurve <- tclVar(0)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Use anchors as curve semilandmarks", variable = e$anchorsCurve), sticky = "w")
    e$curves <- tclVar(0)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Slide semilandmarks on curves", variable = e$curves), sticky = "w")
    e$surfaces <- tclVar(0)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Slide semilandmarks on surfaces", variable = e$surfaces), sticky = "w")
    e$PrinAxes <- tclVar(1)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Align the shape data by principal axes", variable = e$PrinAxes), sticky = "w")
    e$ProcD <- tclVar(1)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Procrustes Distance criterion", variable = e$ProcD), sticky = "w")
    e$Proj <- tclVar(1)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Project into tangent space", variable = e$Proj), sticky = "w")
    e$printP <- tclVar(1)
    tkgrid(tk2checkbutton(gpagenCtlFrame, text = "Print progress bar", variable = e$printP), sticky = "w")

    cmputBtn <- ttkbutton(gpagenCtlFrame, text = "Compute",command = function() compute(e))
    #eoc plot specs btton
    plotspecsBtn <- ttkbutton(gpagenCtlFrame, text = "Plot Aligned Specimens",command = function() plotspecs(e))
    saveBtn <- ttkbutton(gpagenCtlFrame, text = "Save Result",command = function() save(e))
    tkgrid(cmputBtn)
    tkgrid(saveBtn)
    
    #eoc plotting spin box
    tkgrid(tk2label(gpagenCtlFrame,text="Point size"), row = 13, column = 0, sticky="w")
    ##eocpt.cex spinbox
    e$ptcex<- tclVar(.5)
    tkgrid(tk2spinbox(gpagenCtlFrame, from = .1, to = 10, increment = .1
                      ,tip = "Point size", textvariable = e$ptcex, width=5), row = 13, column = 0, sticky="e")
    
    ##eocmean.cex spinbox
    tkgrid(tk2label(gpagenCtlFrame,text="Mean Shape size"), row = 14, column = 0, sticky="w")
    e$meancex<- tclVar(2)
    tkgrid(tk2spinbox(gpagenCtlFrame, from = .5, to = 10, increment = .1
                      ,tip = "Mean Shape size", textvariable = e$meancex, width=5), row = 14, column = 0, sticky="e")
    tkgrid(plotspecsBtn)
    return (gpagenCtlFrame)
}

#configures user button actions
bind.geomorph <-function(e) {   
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
    tkbind(e$canvasFrame, "<Double-Button-1>", function(x, y) {    })
}

updateWidgets.geomorph <- function(e) {
    
}

#checks if given int value is not zero, returns boolean
itob <- function(int) {
    if (int == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#computes shape object data and performs analysis
compute <- function(e) {
    print("compute")
    nSpecimen <- length(e$activeDataList)
    
    coords.lmk <- c()
    coords.lmk <- array(NA, c(as.numeric(e$landmarkNum), 3, nSpecimen))
    #grabs landmarks for ith specimen
    for(i in 1:nSpecimen){
        landmarks <- getLandmark(i)
        if(nrow(landmarks) != as.numeric(e$landmarkNum)) {
            tkmessageBox(title = "Information", message = paste("Incorrect num of landmark for specimen", i), icon = "info", type = "ok")
            return ()
        }
        coords.lmk[,,i] <- landmarks      
    }
    #print(paste("start to compute ... ...")
    #print(coords)
    curves <- NULL
    if (itob(tclvalue(e$curves))) {
        curves <- matrix(e$activeDataList[[1]][[4]],ncol=3)
    }
    # when read from .nts file curves are a list and error is given:  Error in x[s[, 3], ] : invalid subscript type 'list' 
    # It must be unlisted
    # and re-matrixed as so:
    if (is.list(curves)){
        curves<-matrix(unlist(curves),ncol=3)
    }
    surfaces <- NULL
    if (itob(tclvalue(e$surfaces))) {
        coords.A <- array(NA,dim = c(as.numeric(e$sliderNum) + as.numeric(e$landmarkNum),3,nSpecimen))
        surfaces <-matrix((as.numeric(e$landmarkNum) + 1):dim(coords.A)[1],ncol=1)
        for(i in 1:nSpecimen){
            surfaceMatrix <- NULL
            surfaceMatrix <- e$activeDataList[[i]][[8]]
            dim(surfaceMatrix)
            #this is here because the .nts file written to disk is a composite rbound lmk + surfaces
            # this is not the case when we read in a .dgt file.
            if(nrow(surfaceMatrix)!=as.numeric(e$sliderNum)){
                surfaceMatrix<-surfaceMatrix[(as.numeric(e$landmarkNum) + 1):nrow(surfaceMatrix),]
            }
            coords.A[,,i]<-as.matrix(rbind(coords.lmk[,,i],surfaceMatrix))
        } 
    } else {
      
        coords.A <- array(coords.lmk,dim = c(as.numeric(e$landmarkNum),3,nSpecimen))
    }
    print("before gpagen")
    e$gm.results <- gpagen(A=coords.A, 
        curves = curves,
        surfaces = surfaces,
        max.iter = tclvalue(e$maxiter),
        PrinAxes = itob(tclvalue(e$PrinAxes)),
        ProcD = itob(tclvalue(e$ProcD)),
        Proj = itob(tclvalue(e$Proj)),
	print.progress = itob(tclvalue(e$printP)))

    
    pos<-1
    envir <- as.environment(pos)
    assign(x = "gm.results", value = e$gm.results, envir = envir)
    summary(e$gm.results)
}

#saves data as .csv
save <- function(e) {
    filename <- tclvalue(tkgetSaveFile())
    if (nchar(filename)) {
        dfram<-data.frame(Csize=e$gm.results$Csize,coords=two.d.array(e$gm.results$coord))
        write.csv(dfram, paste(filename,".csv",sep=""))	
    }
}

#graphs landmarks in xyz plane
plotspecs <- function(e) {
  geomorph::plotAllSpecimens(gm.results$coords, mean = TRUE, links = NULL, 
                              label = FALSE, plot.param=list(pt.bg="blue", pt.cex=as.numeric(tclvalue(e$ptcex)), mean.bg="red", mean.cex=as.numeric(tclvalue(e$meancex))))
}
