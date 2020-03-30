#calls tkogl2 add function / returns shape object and displays to canvas
add <- function(shape, arg1, arg2, arg3) {
    if (shape == "specimen" || shape == "downsample") {
        tcl("add", shape, arg1, arg2)
    } else if (shape == "dot" || shape == "rawdot" || shape == "rawanchor" || shape == "curve" || shape == "landmark" || shape == "anchor") {
        result <- tclvalue(tcl("add", shape, arg1, arg2, arg3))
        if (startsWith(result, "Error"))
        {
            return(FALSE)
        }
        return(TRUE)
    } else {
        print("Usage: add specimen path_of_ply")
        print("Usage: add dot x y z")
    }
}

#calls tkogl2 set function / assigns gui object to arg value
set <- function(shape, attr, arg1, arg2, arg3) {
    if (shape == "window") {
        if (attr == "id") {
            id<-tkwinfo("id", arg1)
            tcl("setWindow", attr, id)
        }
        else if (attr == "mode") {
            tcl("setWindow", attr, arg1)
        }
        else if (attr == "size") {
            return(tcl("setWindow", attr, arg1, arg2))
        }
    } else if (shape == "specimen") {
        if (attr == "scale" || attr == "allocate" || attr == "id") {
            tcl("setSpecimen", attr, arg1)
        }
        else if (attr == "move") {
            tcl("setSpecimen", attr, arg1, arg2, arg3)
        }
        else if (attr == "angle") {
            msg <- tcl("setSpecimen", attr, arg1, arg2)
            return(msg)
        }
        else {
            print(paste("Not supported attribute", attr))
        }
    } else if (shape == "downsample") {
        return(tclvalue(tcl("setDownSample", attr, arg1, arg2)))

    } else if (shape == "dot") {
        if (attr == "labeled" || attr == "radius" || attr == "anchorRadius" || attr == "alabeled") {
            tcl("setDot", attr, arg1)
        }
        else if (attr == "selected") {
            result <- tcl("setDot", attr, arg1, arg2)
            if (nchar(tclvalue(result)) > 0) {
                return(FALSE)
            }
        }
        else if (attr == "coordinate" || attr == "dcolor" || attr == "acolor" || attr == "anchorColor" || attr == "color") {
            tcl("setDot", attr, arg1, arg2, arg3)
        }
    } else {
        print("Usage: set window [id|size]")
        print("Usage: set specimen [amount|id|scale|angle]")
        print("Usage: set dot [selected|coordinate|color|dcolor|labeled|radius]")
        return(FALSE)
    }
    return(TRUE)
}

#calls tkogl2 shows function / returns string value shape object with specified attributes
shows <- function(shape, attr, arg1, arg2) {
    if (shape == "specimen") {
        if (attr == "xyz") {
            return(tcl("show", shape, attr, arg1, arg2))
        }
    } else if (shape == "landmark") {
        if (attr == "xyz") {
            return(tcl("show", shape, attr, arg1))
        } else if (attr == "id") {
            return(tcl("show", shape, attr))
        }
    } else if (shape == "anchor") {
        if (attr == "xyz") {
            return(tcl("show", shape, attr, arg1))
        } else if (attr == "id") {
            return(tcl("show", shape, attr))
        }
    } else if (shape == "all") {
        msg<-tcl("show", shape)
        return(msg)
    } else {
        print("Usage: shows specimen [xyz] ...")
        print("Usage: shows landmark [reqxyz|id] ...")
        print("Usage: shows all")
    }
}

#calls tkogl2 del function / removes specified shape object
del <- function(shape, arg1, arg2, arg3) {
    if (shape == "dot") {
        tcl("del", shape)
    }
    else if (shape == "dots") {
      tcl("del", shape)
    }
    else if (shape == "anchor") {
        tcl("del", shape)
    }
    else if (shape == "anchors") {
      tcl("del", shape)
    }
    else if(shape == "specimens") {
      tcl("del", shape)
    }
    else {
        print("Usage: del dot [...]")
    }
}

#' GUImorph
#' @export
GUImorph <-function() {
    e <- new.env()
    class(e) <- "main"
    ui(e)
    init(e)
}

#' loadDGT
#' @export
loadDgt <-function(fileName) {
    return(tcl("loadDgt", fileName))
}

#loads tkogl2 functions during loading
.onLoad <- function(libname, pkgname) {
    chname <- "tkogl2"
    file.ext <- .Platform$dynlib.ext #dll file
    dlname <- paste(chname, file.ext, sep = "")
    if (is.character(.Platform$r_arch) && .Platform$r_arch != "")
        path <- file.path("libs", .Platform$r_arc, dlname)
    else path <- file.path("libs", dlname)
    file <- system.file(path, package = pkgname, lib.loc = libname)[1] #grabs full file name
    print(file)
    tryCatch(tcl("load", "C:/Users/amlut/Desktop/Research/GUIMORPH/guimorph_austin1/GUImorph/inst/libs/x64/tkogl2.dll", "Tkogl2"),
             error = function(e)
             warning("loading tkogl2 failed", call. = FALSE)) #replace directory with file
}
