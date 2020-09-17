### Auto-cleanup of CyTOF FCS
### Credit for GUI code: https://github.com/JinmiaoChenLab/cytofkit
### Thanks to El-ad of Astrolabe for his advice on using density function

# Run: cytofclean_GUI()

##################
### Packages
##################

# if (!require("tcltk2")) {
#  install.packages("tcltk2", dependencies = TRUE)
#  library(tcltk2)
# }
# if (!require("flowCore")) {
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#    BiocManager::install("flowCore")
#    library(flowCore)
# }
# if (!require("ggplot2")) {
#  install.packages("ggplot2", dependencies = TRUE)
#  library(ggplot2)
# }
# if (!require("cowplot")) {
#  install.packages("cowplot", dependencies = TRUE)
#  library(cowplot)
# }
# if (!require("scales")) {
#  install.packages("scales", dependencies = TRUE)
#  library(scales)
# }
# if (!require("stats")) {
#  install.packages("stats", dependencies = TRUE)
#  library(stats)
# }


cytofclean_GUI <- function(){

#########################################################
### Parameters
#########################################################

fcsFiles <- ""
cur_dir <- getwd()

#rawFCSdir <- tclVar(cur_dir)
#fcsFile <- tclVar("")
fcsFile <- tclVar(cur_dir)
#resDir <- tclVar(cur_dir)
ret_var <- tclVar("")
# Default is enable bead removal
gbvalue <- tclVar("1")


#########################################################
### Button functions
#########################################################

#reset_rawFCS_dir <- function() {
#  rawFCS_dir <- ""
#  rawFCS_dir <- tclvalue(tkchooseDirectory(title = "Choose your FCS directory ..."))
#  if (rawFCS_dir != "") {
#    tclvalue(rawFCSdir) <- rawFCS_dir
    #tclvalue(resDir) <- rawFCS_dir
#  }
#}

#reset_res_dir <- function() {
#  res_dir <- ""
#  res_dir <- tclvalue(tkchooseDirectory(title = "Choose the output directory ..."))
#  if (res_dir != "") {
#    tclvalue(resDir) <- res_dir
#  }
#}

reset_fcs_data <- function() {
  fnames <- ""
  fnames <- tk_choose.files(default = paste(tclVar(cur_dir),
                                            "fcs", sep = .Platform$file.sep), caption = "Select FCS files",
                            multi = TRUE, filters = matrix(c("{fcs files}", "{.fcs}"),
                                                           1, 2), index = 1)
  if (length(fnames) >= 1) {
    fnames <- fnames[!(grepl(paste0(.Platform$file.sep,
                                    "fcs$"), fnames))]  # remove empty .fcs files
    tclvalue(fcsFile) <- paste(fnames, collapse = "}{")
  }
}

submit <- function() {
  has_error = FALSE
  if (grepl(".fcs$|.FCS$" ,sub('.*(?=.{4}$)', '', tclvalue(fcsFile), perl=T)) == FALSE) {
    tkmessageBox(title = "Error!",
                 message = "Please select some files first.", type = "ok")
    has_error = TRUE
  }

  if (has_error == FALSE) {
    tclvalue(ret_var) <- "OK"
    tkdestroy(tt)
  }
}

quit <- function() {

  tkdestroy(tt)
}


#########################################################
### BUILD GUI
#########################################################


## head line
tt <- tktoplevel(borderwidth = 20)
tkwm.title(tt, "CyTOFClean: Quick Data Cleanup for Mass Cytometry Data")

if(.Platform$OS.type == "windows"){
  box_length <- 63
}else{
  box_length <- 55
}
cell_width <- 3
bt_width <- 8


## rawFCSdir
#rawFCSdir_label <- tklabel(tt, text = "FCS Directory :")
#rawFCSdir_entry <- tkentry(tt, textvariable = rawFCSdir, width = box_length)
#rawFCSdir_button <- tkbutton(tt, text = " Choose... ", width = bt_width, command = reset_rawFCS_dir)


## resDir
#resDir_label <- tklabel(tt, text = "Output Directory :")
#resDir_entry <- tkentry(tt, textvariable = resDir, width = box_length)
#resDir_button <- tkbutton(tt, text = " Choose... ", width = bt_width,
#                          command = reset_res_dir)


## fcsFiles
fcsFile_label <- tklabel(tt, text = "FCS File(s) :")
fcsFile_entry <- tkentry(tt, textvariable = fcsFile, width = box_length)
fcsFile_button <- tkbutton(tt, text = " Select... ", width = bt_width,
                           command = reset_fcs_data)

## submit / reset / quit / Beads checkbox
submit_button <- tkbutton(tt, text = "Process Files...", command = submit)
quit_button <- tkbutton(tt, text = "Quit", command = quit)
gate_beads <- tkcheckbutton(tt,text="Remove Beads", variable=gbvalue, state="active")

#########################################################
### Display GUI
#########################################################

### Input Directory Choice
#tkgrid(rawFCSdir_label, rawFCSdir_entry, rawFCSdir_button,
#       padx = cell_width)
#tkgrid.configure(rawFCSdir_label, rawFCSdir_entry,
#                 sticky = "e")

### File Choice
tkgrid(fcsFile_label,  fcsFile_entry, fcsFile_button,
       padx = cell_width)
tkgrid.configure(fcsFile_label, fcsFile_entry,
                 sticky = "e")

### Output Directory Choice
#tkgrid(resDir_label,  resDir_entry, resDir_button,
#       padx = cell_width)
#tkgrid.configure(resDir_label, resDir_entry, resDir_button,
#                 sticky = "e")

tkgrid(tklabel(tt, text = "\n"), padx = cell_width)  # leave blank line

tkgrid(tklabel(tt, text = ""),tklabel(tt, text = "NOTE: Only Helios / CyTOF v3 files are supported!"))
tkgrid(tklabel(tt, text = ""),tklabel(tt, text = "Files should be normalised before using CyTOFClean."))
tkgrid(tklabel(tt, text = ""),tklabel(tt, text = "Use caution with bead removal if all bead channels contain markers!"))


tkgrid(tklabel(tt, text = "\n"), padx = cell_width)  # leave blank line

tkgrid(tklabel(tt, text = ""), gate_beads, tklabel(tt, text = ""),padx = cell_width)

### Go and Quit buttons
tkgrid(tklabel(tt, text = ""), submit_button,
       quit_button, padx = cell_width)
tkgrid.configure(quit_button, sticky = "w")
tkgrid(tklabel(tt, text = ""),tklabel(tt, text = ""),tklabel(tt, text = "Version: 1.0.1 beta"))

tkwait.window(tt)


########################
### Go button pressed
########################

if (tclvalue(ret_var) != "OK") {
  okMessage <- "Processing cancelled."
}else{
  # Split file list into files
  fcsFiles <- strsplit(tclvalue(fcsFile), "}{", fixed = TRUE)[[1]]
  # Convert to filenames
  filesToOpen <- basename(fcsFiles)
  # Set wd to location of fcs files
  setwd(dirname(fcsFiles[1]))


  # Get total file size
  TotalFileSize <- round(sum(file.size(filesToOpen))/(1024*1024),0)

  # Start timer
  start_time <- Sys.time()

  # create progress bar
  pb <- tkProgressBar(title = "CyTOFClean Progress ...", min = 0,
                      max = 11, width = 300)

  # Advance progress bar
  setTkProgressBar(pb, 1, label="Loading Files ...")


  # Read the files into a flowset
  fcs_raw <- read.flowSet(filesToOpen, transformation = FALSE,
                          truncate_max_range = FALSE)

  # Get parameters
  # Note that this assumes that all files have the same parameters!
  # Possible issues if they differ...
  params <- pData(parameters(fcs_raw[[1]]))

  #Find Gaussian parameter positions
  GaussianPos <- grep("Center|Offset|Width|Residual", params$name)
  # Find Time position
  TimePos <- grep("Time", params$name)

  # Arcsinh transform the Gaussians into a new data frame, since we don't want to affect the actual data
  # Also scale approximating cytobank
  cofactor <- 5
  Scale <- c(-5,12000)
  # Bodge to get approx. figures for cytobank
  # Center/Offset/Width/Residual
  cytobank_scale <- c(10,80,80,40)
  FCSDATAGaussians <- NULL
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- as.data.frame(asinh(exprs(fcs_raw[[i]][,GaussianPos])/cofactor))
     for (j in 1:length(GaussianPos)){
        FCSDATAGaussians[[i]][,j] <- rescale(FCSDATAGaussians[[i]][,j],to = Scale)/cytobank_scale[j]
      }
  }
  # Add Time
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]]$Time <- exprs(fcs_raw[[i]][,TimePos])
  }


  #plot(density(FCSDATAGaussians[[1]]$Center))
  #plot(density(FCSDATAGaussians$Offset))
  #plot(density(FCSDATAGaussians$Width))
  #plot(density(FCSDATAGaussians$Residual))


  # Check for Event_length
  if ("Event_length" %in% params$name == FALSE){
    tkmessageBox(title = "Error!",
                 message = "Only Helios / CyTOF v3 files are supported!", type = "ok")
    # Close progress bar
    close(pb)
    # Open GUI again
    cytofclean_GUI()
  }
  ## Check for 140 Channel
  #if ("Ce140Di" %in% params$name == FALSE){
  #  tkmessageBox(title = "Error!",
  #               message = "Missing EQ Bead (Ce140) Channel!", type = "ok")
  #  # Close progress bar
  #  close(pb)
  #}else{

  ## Check for Any suitable bead channels
  if (sum(grepl("140|151|153|165|175", params$name)) < 1 & tclvalue(gbvalue)==1){
    tkmessageBox(title = "Error!",
                 message = "Missing EQ Bead Channels! Please disable bead removal.", type = "ok")
  # Close progress bar
    close(pb)
    # Open GUI again
    cytofclean_GUI()
  }else{

  # Select bead channels for removal
  UserBeadChannels <- NULL
  if (tclvalue(gbvalue)==1){
    UserBeadChannels <- tk_select.list(params$desc[grep("140Ce|142Ce|151Eu|153Eu|165Ho|175Lu|176Lu", params$desc)], multiple=TRUE,title="Select bead channels that do not contain markers. Hit cancel to use all.")
  }

  # If user cancels dialog box, use all markers.
  if(length(UserBeadChannels)==0 ){
    UserBeadChannels <- params$desc[grep("140Ce|142Ce|151Eu|153Eu|165Ho|175Lu|176Lu", params$desc)]
  }

  # Advance progress bar
  setTkProgressBar(pb, 2, label="Gating Events ...")

  # Get original cell number
  OrigEvents <- fsApply(fcs_raw, nrow)
  #for (i in 1:length(filesToOpen)){
  #  OrigEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  #}

  # Generate random row numbers for subsampling to speed up plotting
  RandEvents <- list()
  if (min(OrigEvents)>10000){
    for (i in 1:length(filesToOpen)){
    RandEvents[i] <- list(sample(1:OrigEvents[i],10000/length(filesToOpen)))
    }
  }else{
    for (i in 1:length(filesToOpen)){
    RandEvents[i] <- list(sample(1:OrigEvents[i],OrigEvents[i]/length(filesToOpen)))
    }
  }


  # Get total acquisition time
  maxtime <- NULL
  for (i in 1:length(filesToOpen)){
    maxtime[i] <- max(exprs(fcs_raw[[i]]$`Time`))
  }

  # Convert to mins
  TimeDiv <- 60 * 1000
  maxtime <- round(maxtime/TimeDiv,2)

  # Create colour scale for plot
  # The extra "black" is needed to make the colour scale appear decent - otherwise it doesn't show
  colfunc <- colorRampPalette(c("black",# "black","black", "black", "black",# "black", "black", "black",
                                "purple4", #"purple4",
                                "red", "yellow"))

  # Get Density of Events
  Event_Density <- NULL
  for (i in 1:length(filesToOpen)){
    Event_Density[[i]] <-  density(exprs(fcs_raw[[i]]$`Event_length`))
  }

  #plot(Event_Density[[1]]$x, Event_Density[[1]]$y, type="l")

  # Set min and max based on spread of density
  # Modified based on https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.23960
  # Suggesting that upper limit should be 95% of Gaussian fit around Mode.
  # However, this seems to drastically remove events
  EventMode <- NULL
  EventMin <- NULL
  EventMax <- NULL
  #EventGaussL <- NULL
  #EventGaussR <- NULL
  #EventSigma <- NULL
  for (i in 1:length(filesToOpen)){
    #EventMode[i] <- Event_Density[[i]]$x[Event_Density[[i]]$y == max(Event_Density[[i]]$y)]
    # Find 68% height of left and right side - to approx. Gaussian
    #EventGaussL[i] <- min(Event_Density[[i]]$x[Event_Density[[i]]$y>max(Event_Density[[i]]$y)*.682])
    #EventGaussR[i] <- max(Event_Density[[i]]$x[Event_Density[[i]]$y>max(Event_Density[[i]]$y)*.682])
    #EventSigma[i] <- (EventMode[i] - mean(EventGaussL[i],EventGaussR[i]))
    #EventMin[i] <- EventMode[i] - 2 * EventSigma[i]
    #EventMax[i] <- EventMode[i] + 2 * EventSigma[i]
    EventMin[i] <- min(Event_Density[[i]]$x[Event_Density[[i]]$y>max(Event_Density[[i]]$y)*.3])
    EventMax[i] <- max(Event_Density[[i]]$x[Event_Density[[i]]$y>max(Event_Density[[i]]$y)*.1])
  }

  # % Cells After Event gating
  AfterEvent <- NULL
  for (i in 1:length(filesToOpen)){
    AfterEvent[i] <- sum(exprs(fcs_raw[[i]]$`Event_length`)<EventMax[i] &
                           exprs(fcs_raw[[i]]$`Event_length`)>EventMin[i])/length(exprs(fcs_raw[[i]]$`Event_length`))
  }


  options(warn=-1)
  ## Plot x as Time and Y as Event Length
  EventPlot <- list()
  for (i in 1:length(filesToOpen)){
  EventPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Event_length)) +
    # Only label 0 and max on X Axis
    scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
    # Plot all points
    geom_point(shape=".",alpha=1)+
    # Fill with transparent colour fill using density stats
    # ndensity scales each graph to its own min/max values
    stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
    # Produces a colour scale based on the colours in the colfunc list
    scale_fill_gradientn(colours=colfunc(128)) +
    # Force Y axis to start at zero and stop at 100
    #ylim(0,100) +
    scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100)) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none")+
    # Draw gate
    geom_rect(xmin=0, ymin=EventMin[i], xmax=maxtime[i], ymax=EventMax[i], colour="red", alpha=0)+
    # Hide Y axis label
    #ylab(NULL) +
    # Change X axis label
    xlab(NULL) +
    #xlab("Time (min)")+
    ggtitle(filesToOpen[i],subtitle = paste(round(AfterEvent[i]*100,1),"%")) +
    theme(plot.title = element_text(size=8, hjust=0.5), plot.subtitle = element_text(size=8))+
    coord_cartesian(expand = FALSE)
  }
  options(warn=0)

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(exprs(fcs_raw[[i]]$`Event_length`) < EventMax[i] & exprs(fcs_raw[[i]]$`Event_length`) > EventMin[i])
  }

  # Gate Gaussian Data
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- FCSDATAGaussians[[i]][EventsToKeep[[i]],]
  }

  #And Gate Events in actual flowSet
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
  }



  # Advance progress bar
  setTkProgressBar(pb, 3, label="Gating Centre ...")

  # Get new event number
  NewEvents <- NULL
  for (i in 1:length(filesToOpen)){
    NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Get new random rows
  if (min(NewEvents)>10000){
    rm(NewEvents)
    rm(RandEvents)
    NewEvents <- NULL
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
      RandEvents[i] <- list(sample(1:NewEvents[i],10000/length(filesToOpen)))
    }
  }else{
    rm(RandEvents)
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      RandEvents[i] <- list(sample(1:NewEvents[i],NewEvents[i]/length(filesToOpen)))
    }
  }

  # Get density of Centre - note English UK vs US spelling!
  Centre_Density <- NULL
  for (i in 1:length(filesToOpen)){
    # Get density plot
    #Centre_Density[[i]] <-  density(exprs(fcs_raw[[i]]$`Center`))
    Centre_Density[[i]] <-  density(FCSDATAGaussians[[i]]$Center)
  }

  # Flatten out the curve to remove the low peak and correct false detection of minimum value
  for (i in 1:length(filesToOpen)){
    Centre_Density[[i]]$y[Centre_Density[[i]]$x < 250] <- 0
  }

  #plot(Centre_Density[[1]]$x, Centre_Density[[1]]$y, log="x")

  # Set min and max based on density spread
  #CentreMode <- NULL
  CentreMin <- NULL
  CentreMax <- NULL
  #CentreSigma <- NULL
  #CentreGauss <- NULL
  for (i in 1:length(filesToOpen)){
    #CentreMode[i] <- Centre_Density[[i]]$x[Centre_Density[[i]]$y == max(Centre_Density[[i]]$y)]
    #CentreGauss[i] <- min(Centre_Density[[i]]$x[Centre_Density[[i]]$y > max(Centre_Density[[i]]$y)*.682])
    #CentreSigma[i] <- CentreMode[i] - CentreGauss[i]
    #CentreMin[i] <- CentreMode[i] - 1.5 * CentreSigma[i]
    #CentreMax[i] <- CentreMode[i] + 1.5 * CentreSigma[i]
    CentreMin[i] <- min(Centre_Density[[i]]$x[Centre_Density[[i]]$y > max(Centre_Density[[i]]$y)*.05])
    CentreMax[i] <- max(Centre_Density[[i]]$x[Centre_Density[[i]]$y > max(Centre_Density[[i]]$y)*.05])
  }


  # Clip any min to a low value - this helps with very "dirty" samples
  #CentreMin[CentreMin < 300] <- 300

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(FCSDATAGaussians[[i]]$Center < CentreMax[i] & FCSDATAGaussians[[i]]$Center > CentreMin[i])
  }

  # % Cells After Centre gating
  AfterCentre <- NULL
  for (i in 1:length(filesToOpen)){
  #  AfterCentre[i] <- sum(exprs(fcs_raw[[i]]$`Center`) < CentreMax[i] & exprs(fcs_raw[[i]]$`Center`) > CentreMin[i]) / length(exprs(fcs_raw[[i]]$`Center`))
    AfterCentre[i] <- length(EventsToKeep[[i]]) / length(exprs(fcs_raw[[i]]$`Center`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    #FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterCentre[i]
    #FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
    FinalEvents[i] <- length(EventsToKeep[[i]])/OrigEvents[i]
  }

  # Set Y axis for plots suitably
  #CentreYAxis <- round((mean(CentreMax)-mean(CentreMin)) * 5,-2)
  CentreYAxis <- round(max(CentreMax) * 1.2,-2)


  options(warn=-1)
  ## Plot x as Time and Y as Center
  CentrePlot <- list()
  for (i in 1:length(filesToOpen)){
  #CentrePlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Center)) +
  CentrePlot[[i]] <- ggplot(FCSDATAGaussians[[i]][RandEvents[[i]],], aes(x=Time/TimeDiv, y=Center)) +
    # Only label 0 and max on X Axis
    scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
    # Plot all points
    geom_point(shape=".",alpha=1)+
    # Fill with transparent colour fill using density stats
    # ndensity scales each graph to its own min/max values
    stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
    # Produces a colour scale based on the colours in the colfunc list
    scale_fill_gradientn(colours=colfunc(128)) +
    # Force Y axis to start at zero and stop at maxEvent
    ylim(0,CentreYAxis) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none") +
  # Draw gate
  geom_rect(xmin=0, ymin=CentreMin[i], xmax=maxtime[i], ymax=CentreMax[i], colour="red", alpha=0)+
  # Hide Y axis label
  #ylab(NULL) +
    # Change X axis label
    xlab(NULL) +
    #xlab("Time (min)")+
  ggtitle(paste(round(AfterCentre[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep=""))  +
    theme(plot.title = element_text(size=8))+
    coord_cartesian(expand = FALSE)
  }
  options(warn=0)

  # Gate Centre
  for (i in 1:length(filesToOpen)){
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Center`)<CentreMax[i] & exprs(fcs_raw[[i]]$`Center`)>CentreMin[i]]
    fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
  }

  # Gate Gaussian Data
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- FCSDATAGaussians[[i]][EventsToKeep[[i]],]
  }

  # Advance progress bar
  setTkProgressBar(pb, 4, label="Gating Offset ...")

  # Get new event number
  NewEvents <- NULL
  for (i in 1:length(filesToOpen)){
    NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Get new random rows
  if (min(NewEvents)>10000){
    rm(NewEvents)
    rm(RandEvents)
    NewEvents <- NULL
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
      RandEvents[i] <- list(sample(1:NewEvents[i],10000/length(filesToOpen)))
    }
  }else{
    rm(RandEvents)
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      RandEvents[i] <- list(sample(1:NewEvents[i],NewEvents[i]/length(filesToOpen)))
    }
  }


  # Get density plot of Offset
  Offset_Density <- NULL
  for (i in 1:length(filesToOpen)){
    #Offset_Density[[i]] <-  density(exprs(fcs_raw[[i]]$`Offset`))
    Offset_Density[[i]] <-  density(FCSDATAGaussians[[i]]$Offset)
  }

  # Flatten out the curve to remove the low peak and correct false detection of minimum value
  for (i in 1:length(filesToOpen)){
    Offset_Density[[i]]$y[Offset_Density[[i]]$x < 40] <- 0
  }

  #plot(Offset_Density[[1]]$x, Offset_Density[[1]]$y, log="x")

  # Set min and max based on spread
  OffsetMin <- NULL
  OffsetMax <- NULL
  #OffsetMode <- NULL
  #OffsetGauss <- NULL
  #OffsetSigma <- NULL
  for (i in 1:length(filesToOpen)){
    #OffsetMode[i] <- Offset_Density[[i]]$x[Offset_Density[[i]]$y == max(Offset_Density[[i]]$y)]
    #OffsetGauss[i] <- min(Offset_Density[[i]]$x[Offset_Density[[i]]$y > max(Offset_Density[[i]]$y)*.682])
    #OffsetSigma[i] <- OffsetMode[i] - OffsetGauss[i]
    #OffsetMin[i] <- OffsetMode[i] - 2.5 * OffsetSigma[i]
    #OffsetMax[i] <- OffsetMode[i] + 2.5 * OffsetSigma[i]
    OffsetMin[i] <- min(Offset_Density[[i]]$x[Offset_Density[[i]]$y>max(Offset_Density[[i]]$y)*.05])
    OffsetMax[i] <- max(Offset_Density[[i]]$x[Offset_Density[[i]]$y>max(Offset_Density[[i]]$y)*.05])
  }


  # Clip any min to a low value - just in case density detection has found an incorrect location - this happens in bad datasets
  #OffsetMin[OffsetMin < 50] <- 50

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(FCSDATAGaussians[[i]]$Offset < OffsetMax[i] & FCSDATAGaussians[[i]]$Offset > OffsetMin[i])
  }

  # % Cells After Offset gating
  AfterOffset <- NULL
  for (i in 1:length(filesToOpen)){
    #AfterOffset[i] <- sum(exprs(fcs_raw[[i]]$`Offset`)<OffsetMax[i] & exprs(fcs_raw[[i]]$`Offset`)>OffsetMin[i])/length(exprs(fcs_raw[[i]]$`Offset`))
    AfterOffset[i] <- length(EventsToKeep[[i]]) / length(exprs(fcs_raw[[i]]$`Offset`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    #FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterOffset[i]
    #FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
    FinalEvents[i] <- length(EventsToKeep[[i]]) / OrigEvents[i]
  }

  # Get Y axis for plots
  #OffsetYAxis <- round((mean(OffsetMax) - mean(OffsetMin)) * 5,-1)
  OffsetYAxis <- round(mean(OffsetMax) * 1.2,-1)

  options(warn=-1)
  ## Plot x as Time and Y as Offset
  OffsetPlot <- list()
  for (i in 1:length(filesToOpen)){
  #OffsetPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Offset)) +
  OffsetPlot[[i]] <- ggplot(FCSDATAGaussians[[i]][RandEvents[[i]],], aes(x=Time/TimeDiv, y=Offset)) +
    # Only label 0 and max on X Axis
    scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
    # Plot all points
    geom_point(shape=".",alpha=0.5)+
    # Fill with transparent colour fill using density stats
    # ndensity scales each graph to its own min/max values
    stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
    # Produces a colour scale based on the colours in the colfunc list
    scale_fill_gradientn(colours=colfunc(128)) +
    # Force Y axis to start at zero and stop at maxEvent
    ylim(0,OffsetYAxis) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none") +
    # Draw gate
    geom_rect(xmin=0, ymin=OffsetMin[i], xmax=maxtime[i], ymax=OffsetMax[i], colour="red", alpha=0)+
  # Hide Y axis label
  #ylab(NULL) +
    # Change X axis label
    xlab(NULL) +
    #xlab("Time (min)")+
  ggtitle(paste(round(AfterOffset[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep="")) +
    theme(plot.title = element_text(size=8))+
    coord_cartesian(expand = FALSE)
  }
  options(warn=0)

  # Gate Centre
  for (i in 1:length(filesToOpen)){
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Offset`)<OffsetMax[i] & exprs(fcs_raw[[i]]$`Offset`)>OffsetMin[i]]
    fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
  }

  # Gate Gaussian Data
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- FCSDATAGaussians[[i]][EventsToKeep[[i]],]
  }

  # Advance progress bar
  setTkProgressBar(pb, 5, label="Gating Residual ...")

  # Get new event number
  NewEvents <- NULL
  for (i in 1:length(filesToOpen)){
    NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Get new random rows
  if (min(NewEvents)>10000){
    rm(NewEvents)
    rm(RandEvents)
    NewEvents <- NULL
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
      RandEvents[i] <- list(sample(1:NewEvents[i],10000/length(filesToOpen)))
    }
  }else{
    rm(RandEvents)
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      RandEvents[i] <- list(sample(1:NewEvents[i],NewEvents[i]/length(filesToOpen)))
    }
  }

  Residual_Density <- NULL
  for (i in 1:length(filesToOpen)){
    #Residual_Density[[i]] <-  density(exprs(fcs_raw[[i]]$`Residual`))
    Residual_Density[[i]] <-  density(FCSDATAGaussians[[i]]$Residual)
  }

  # Mode, Gauss and Sigma work well on most data, but not concatenated where there are breaks!
  #ResidualMode <- NULL
  #ResidualGauss <- NULL
  #ResidualSigma <- NULL
  ResidualMin <- NULL
  ResidualMax <- NULL
  for (i in 1:length(filesToOpen)){
    #ResidualMode[i] <- Residual_Density[[i]]$x[Residual_Density[[i]]$y == max(Residual_Density[[i]]$y)]
    #ResidualGauss[i] <- min(Residual_Density[[i]]$x[Residual_Density[[i]]$y > max(Residual_Density[[i]]$y)*.682])
    #ResidualSigma[i] <- ResidualMode[i] - ResidualGauss[i]
    #ResidualMin[i] <- ResidualMode[i] - 2 * ResidualSigma[i]
    #ResidualMax[i] <- ResidualMode[i] + 2.5 * ResidualSigma[i]
    ResidualMin[i] <- min(Residual_Density[[i]]$x[Residual_Density[[i]]$y>max(Residual_Density[[i]]$y)*.05])
    ResidualMax[i] <- max(Residual_Density[[i]]$x[Residual_Density[[i]]$y>max(Residual_Density[[i]]$y)*.05])
  }

  #plot(Residual_Density[[1]]$x, Residual_Density[[1]]$y, log="x")


  # Clip any low values - just in case density detection has found a wrong value
  ResidualMin[ResidualMin<0] <- 0

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(FCSDATAGaussians[[i]]$Residual < ResidualMax[i] & FCSDATAGaussians[[i]]$Residual > ResidualMin[i])
  }

  # % Cells After Residual gating
  AfterResidual <- NULL
  for (i in 1:length(filesToOpen)){
    #AfterResidual[i] <- sum(exprs(fcs_raw[[i]]$`Residual`)<ResidualMax[i] & exprs(fcs_raw[[i]]$`Residual`)>ResidualMin[i])/length(exprs(fcs_raw[[i]]$`Residual`))
    AfterResidual[i] <- length(EventsToKeep[[i]]) / length(exprs(fcs_raw[[i]]$`Residual`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    #FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterResidual[i]
    #FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
    FinalEvents[i] <- length(EventsToKeep[[i]]) / OrigEvents[i]
  }

  # Get Scale for Y axis
  #ResidualYAxis <- round((mean(ResidualMax)-mean(ResidualMin)) * 3,-2)
  ResidualYAxis <- round(mean(ResidualMax) * 1.2, -1)

  options(warn=-1)
  ## Plot x as Time and Y as Residual
  ResidualPlot <- list()
  for (i in 1:length(filesToOpen)){
  #ResidualPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Residual)) +
    ResidualPlot[[i]] <- ggplot(FCSDATAGaussians[[i]][RandEvents[[i]],], aes(x=Time/TimeDiv, y=Residual)) +
    # Only label 0 and max on X Axis
    scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
    # Plot all points
    geom_point(shape=".",alpha=0.5)+
    # Fill with transparent colour fill using density stats
    # ndensity scales each graph to its own min/max values
    stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
    # Produces a colour scale based on the colours in the colfunc list
    scale_fill_gradientn(colours=colfunc(128)) +
    # Force Y axis to start at zero and stop at maxEvent
    ylim(0,ResidualYAxis) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none") +
    # Draw gate
    geom_rect(xmin=0, ymin=ResidualMin[i], xmax=maxtime[i], ymax=ResidualMax[i], colour="red", alpha=0)+
  # Hide Y axis label
  #ylab(NULL) +
    # Change X axis label
    xlab(NULL) +
    #xlab("Time (min)")+
  ggtitle(paste(round(AfterResidual[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep="")) +
    theme(plot.title = element_text(size=8))+
      coord_cartesian(expand = FALSE)
  }
  options(warn=0)

  # Gate Residual
  for (i in 1:length(filesToOpen)){
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Residual`)<ResidualMax[i]& exprs(fcs_raw[[i]]$`Residual`)>ResidualMin[i]]
    fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
  }

  # Gate Gaussian Data
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- FCSDATAGaussians[[i]][EventsToKeep[[i]],]
  }


  # Advance progress bar
  setTkProgressBar(pb, 6, label="Gating Width ...")

  # Get new event number
  NewEvents <- NULL
  for (i in 1:length(filesToOpen)){
    NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Get new random rows
  if (min(NewEvents)>10000){
    rm(NewEvents)
    rm(RandEvents)
    NewEvents <- NULL
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
      RandEvents[i] <- list(sample(1:NewEvents[i],10000/length(filesToOpen)))
    }
  }else{
    rm(RandEvents)
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      RandEvents[i] <- list(sample(1:NewEvents[i],NewEvents[i]/length(filesToOpen)))
    }
  }

  Width_Density <- NULL
  for (i in 1:length(filesToOpen)){
    #Width_Density[[i]] <-  density(exprs(fcs_raw[[i]]$`Width`))
    Width_Density[[i]] <-  density(FCSDATAGaussians[[i]]$Width)
  }

  # Flatten out the curve to remove the low peak and correct false detection of minimum value
  #for (i in 1:length(filesToOpen)){
  #  Width_Density[[i]]$y[Width_Density[[i]]$x < 30] <- 0
  #}


  #plot(Width_Density[[1]]$x, Width_Density[[1]]$y, log="x")

  # NOTE - unlike other parameters, Width has a 3x Sigma for the max.
  #WidthMode <- NULL
  #WidthGauss <- NULL
  #WidthSigma <- NULL
  WidthMin <- NULL
  WidthMax <- NULL
  for (i in 1:length(filesToOpen)){
    #WidthMode[i] <- Width_Density[[i]]$x[Width_Density[[i]]$y == max(Width_Density[[i]]$y)]
    #WidthGauss[i] <- min(Width_Density[[i]]$x[Width_Density[[i]]$y > max(Width_Density[[i]]$y)*.682])
    #WidthSigma[i] <- WidthMode[i] - WidthGauss[i]
    #WidthMin[i] <- WidthMode[i] - 2 * WidthSigma[i]
    #WidthMax[i] <- WidthMode[i] + 3 * WidthSigma[i]
    WidthMin[i] <- min(Width_Density[[i]]$x[Width_Density[[i]]$y > max(Width_Density[[i]]$y)*.05])
    WidthMax[i] <- max(Width_Density[[i]]$x[Width_Density[[i]]$y > max(Width_Density[[i]]$y)*.05])
  }


  # Clip any low min to a low value - useful for bad datasets
  #WidthMin[WidthMin < 30] <- 30

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(FCSDATAGaussians[[i]]$Width < WidthMax[i] & FCSDATAGaussians[[i]]$Width > WidthMin[i])
  }

  # % Cells After Residual gating
  AfterWidth <- NULL
  for (i in 1:length(filesToOpen)){
    #AfterWidth[i] <- sum(exprs(fcs_raw[[i]]$`Width`) < WidthMax[i] & exprs(fcs_raw[[i]]$`Width`) > WidthMin[i])/length(exprs(fcs_raw[[i]]$`Width`))
    AfterWidth[i] <- length(EventsToKeep[[i]]) / length(exprs(fcs_raw[[i]]$`Width`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    #FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterWidth[i]
    #FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
    FinalEvents[i] <- length(EventsToKeep[[i]]) /OrigEvents[i]
  }

  # Get Mode for Y axis
  #WidthYAxis <- round((mean(WidthMax)-mean(WidthMin)) * 4,-2)
  WidthYAxis <- round(mean(WidthMax) * 1.2,-1)

  options(warn=-1)
  ## Plot x as Time and Y as Width
  WidthPlot <- list()
  for (i in 1:length(filesToOpen)){
    #WidthPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Width)) +
    WidthPlot[[i]] <- ggplot(FCSDATAGaussians[[i]][RandEvents[[i]],], aes(x=Time/TimeDiv, y=Width)) +
      # Only label 0 and max on X Axis
      scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
      # Plot all points
      geom_point(shape=".",alpha=0.5)+
      # Fill with transparent colour fill using density stats
      # ndensity scales each graph to its own min/max values
      stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
      # Produces a colour scale based on the colours in the colfunc list
      scale_fill_gradientn(colours=colfunc(128)) +
      # Force Y axis to start at zero and stop at maxEvent
      ylim(0,WidthYAxis) +
      # Hide Y axis values if desired
      #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
      # Hide legend
      theme(legend.position = "none") +
      # Draw gate
      geom_rect(xmin=0, ymin=WidthMin[i], xmax=maxtime[i], ymax=WidthMax[i], colour="red", alpha=0)+
      # Hide Y axis label
      #ylab(NULL) +
      # Change X axis label
      xlab(NULL) +
      #xlab("Time (min)")+
      ggtitle(paste(round(AfterWidth[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep="")) +
      theme(plot.title = element_text(size=8))+
      coord_cartesian(expand=FALSE)
  }
  options(warn=0)

  # Gate Width
  for (i in 1:length(filesToOpen)){
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Width`) < WidthMax[i] & exprs(fcs_raw[[i]]$`Width`) > WidthMin[i]]
    fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
  }


  # Gate Gaussian Data
  for (i in 1:length(filesToOpen)){
    FCSDATAGaussians[[i]] <- FCSDATAGaussians[[i]][EventsToKeep[[i]],]
  }


  # Only proceed if bead removal enabled
  if (tclvalue(gbvalue)==1){
  # Advance progress bar
  setTkProgressBar(pb, 7, label="Removing Beads ...")

  # Get new event number
  NewEvents <- NULL
  for (i in 1:length(filesToOpen)){
    NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Get new random rows
  if (min(NewEvents)>10000){
    rm(NewEvents)
    rm(RandEvents)
    NewEvents <- NULL
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      NewEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
      RandEvents[i] <- list(sample(1:NewEvents[i],10000/length(filesToOpen)))
    }
  }else{
    rm(RandEvents)
    RandEvents <- NULL
    for (i in 1:length(filesToOpen)){
      RandEvents[i] <- list(sample(1:NewEvents[i],NewEvents[i]/length(filesToOpen)))
    }
  }

  # Get Bead Channels present
  #Bead_Channels <- params$name[grepl("140|151|153|165|175", params$name)]
  # Or limit to only those selected by users
  # Convert UserBeadChannels to "grepable" list
  UserBeadChannels <- paste(as.numeric(substr(UserBeadChannels,1,3)), collapse = "|")
  Bead_Channels <- params$name[grepl(UserBeadChannels,params$name)]

  # Create new transformed data for bead channels
  FCSDATABeads <- NULL
  for (i in 1:length(filesToOpen)){
    FCSDATABeads[[i]] <- as.data.frame(asinh(exprs(fcs_raw[[i]][,Bead_Channels])/cofactor))
    for (j in 1:length(Bead_Channels)){
      FCSDATABeads[[i]][,j] <- rescale(FCSDATABeads[[i]][,j],to = Scale)
    }
  }
  # Add Time
  for (i in 1:length(filesToOpen)){
    FCSDATABeads[[i]]$Time <- exprs(fcs_raw[[i]][,TimePos])
  }

  # Calculate density of these channels for each file
  Bead_Density <- NULL
  for (j in Bead_Channels){
    for (i in 1:length(filesToOpen)){
    #Bead_Density[[i]] <-  density(exprs(fcs_raw[[i]][,j]))
    Bead_Density[[i]] <- density(FCSDATABeads[[i]][,j])
    # Flatten the curves for the areas likely to be cells and above the bead region
    #Bead_Density[[i]]$y[Bead_Density[[i]]$x<150] <- 0.001
    #Bead_Density[[i]]$y[Bead_Density[[i]]$x>3000] <- 0.001
    Bead_Density[[i]]$y[Bead_Density[[i]]$x<5000] <- 0
    }
    # Name as per bead channel
    assign(j, Bead_Density)
  }

  #Plots:
  #plot(Ce140Di[[1]]$x,Ce140Di[[1]]$y, log="x")
  #plot(Ce142Di[[1]]$x,Ce142Di[[1]]$y, log="x")
  #plot(Eu151Di[[1]]$x,Eu151Di[[1]]$y, log="x")
  #plot(Eu153Di[[1]]$x,Eu153Di[[1]]$y, log="x")
  #plot(Ho165Di[[1]]$x,Ho165Di[[1]]$y, log="x")
  #plot(Lu175Di[[1]]$x,Lu175Di[[1]]$y, log="x")
  #plot(Lu176Di[[1]]$x,Lu176Di[[1]]$y, log="x")



  # Find the "cleanest" bead channel to use for bead gating -
  # i.e. the one with the lowest density in the region between cells and beads
  #MinCellPeak <- NULL
  #for (j in Bead_Channels){
  #  for (i in 1:length(filesToOpen)){
  #  MinCellPeak[i] <- min(get(j)[[i]]$y)
  #  }
  #  assign(j, MinCellPeak)
  #}
  # Now using asinh data, the sharpness of the peak is better
  BeadPeak <- NULL
  for (j in Bead_Channels){
    for (i in 1:length(filesToOpen)){
      BeadPeak[i]<- max(get(j)[[i]]$y)
    }
    # Rename as per bead channel
    assign(paste0(j,"_BeadPeak"), BeadPeak)
  }
  #Find FWHM (actually at 25%)
  BeadFWHM <- NULL
  for (j in Bead_Channels){
    for (i in 1:length(filesToOpen)){
      BeadFWHM[i] <- max(get(j)[[i]]$x[get(j)[[i]]$y > get(paste0(j,"_BeadPeak"))[[i]]/4]) - min(get(j)[[i]]$x[get(j)[[i]]$y > get(paste0(j,"_BeadPeak"))[[i]]/4])
    }
    assign(paste0(j,"_FWHM"), BeadFWHM)
  }

  # Mean across all files
  #for (i in 1:length(Bead_Channels)){
  #  MinCellPeak[i] <- mean(get(Bead_Channels[i]))
  #}
  MeanFWHM <- NULL
  for (i in Bead_Channels){
    MeanFWHM[i] <- mean(get(paste0(i,"_FWHM")))
  }


  # Channel to use as Bead Gate
  #Channel_To_Gate <- Bead_Channels[grepl(min(MinCellPeak), MinCellPeak)]
  Channel_To_Gate <- Bead_Channels[grepl(min(MeanFWHM), MeanFWHM)]

  # Just in case more than one detected
  Channel_To_Gate <- Channel_To_Gate[1]

  #Recreate Bead Density for this Channel Only
  Bead_Density <- NULL
    for (i in 1:length(filesToOpen)){
      #Bead_Density[[i]] <-  density(exprs(fcs_raw[[i]][,Channel_To_Gate]))
      Bead_Density[[i]] <-  density(FCSDATABeads[[i]][,Channel_To_Gate])
      # Flatten the curves for the areas likely to be cells and above the bead region
      #Bead_Density[[i]]$y[Bead_Density[[i]]$x<150] <- 0.001
      #Bead_Density[[i]]$y[Bead_Density[[i]]$x>3000] <- 0.001
      #Bead_Density[[i]]$y[Bead_Density[[i]]$x<5000] <- 0
      Bead_Density[[i]]$y[Bead_Density[[i]]$x<100] <- 0
    }

  #plot(Bead_Density[[5]]$x, Bead_Density[[5]]$y, log="x")

  # Find centre of point between beads and cells
  BeadMax <- NULL
  CellMax <- NULL
  #NotBeadMax <- NULL
  #NotBeadMid <- NULL
  #NotBeadMin <- NULL
  for (i in 1:length(filesToOpen)){
    #NotBeadMax[i] <- min(Bead_Density[[i]]$x[Bead_Density[[i]]$y == max(Bead_Density[[i]]$y)])
   #NotBeadMax[i] <- min(Bead_Density[[i]]$x[Bead_Density[[i]]$y > max(Bead_Density[[i]]$y)*0.05])
    #NotBeadMid[i] <- min(Bead_Density[[i]]$x[Bead_Density[[i]]$y > max(Bead_Density[[i]]$y)*.5])
    #NotBeadMin[i] <- min(Bead_Density[[i]]$x[Bead_Density[[i]]$y > max(Bead_Density[[i]]$y)*.05])
    BeadMax[i] <- Bead_Density[[i]]$x[Bead_Density[[i]]$y == max(Bead_Density[[i]]$y[Bead_Density[[i]]$x>5000])]
    CellMax[i] <- Bead_Density[[i]]$x[Bead_Density[[i]]$y == max(Bead_Density[[i]]$y[Bead_Density[[i]]$x<5000])]
  }

  # Set the cutoff point to be a multiple of this width
  #NotBeadMax <- NotBeadMax - ((NotBeadMax - NotBeadMid) * 10)

  # Set at midpoint between min and max
  #NotBeadMax <- mean(c(NotBeadMin,NotBeadMax))

  # Set cutoff to be halfway between cells and beads
  NotBeadMax <- colMeans(rbind(CellMax, BeadMax))

  # Get List of Rows (Events) to keep
  EventsToKeep <- NULL
  for (i in 1:length(filesToOpen)){
    EventsToKeep[[i]] <- which(FCSDATABeads[[i]][,Channel_To_Gate] < NotBeadMax[i])
  }

  NotBeadsPop <- NULL
  for (i in 1:length(filesToOpen)){
    # % Cells After Not-Beads gating
    #NotBeadsPop[i] <- sum(exprs(fcs_raw[[i]][,Channel_To_Gate]) < NotBeadMax[i])/length(exprs(fcs_raw[[i]][,Channel_To_Gate]))
    NotBeadsPop[i] <- length(EventsToKeep[[i]]) / length(exprs(fcs_raw[[i]][,Channel_To_Gate]))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    #FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * NotBeadsPop[i]
    #FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
    FinalEvents[i] <- length(EventsToKeep[[i]]) / OrigEvents[i]
  }


  # Needed to create a new data frame in order to plot the Bead Channel
  # Couldn't find a way to reference the Bead_Channel as the y variable in the ggplot aes code
  #Bead_Plot_Data <- as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],c("Time", Channel_To_Gate)]))
  Bead_Plot_Data <- FCSDATABeads[[i]][RandEvents[[i]],c("Time", Channel_To_Gate)]

  options(warn=-1)
  ## Plot x as Time and Y as bead channel
  NotBeadsPlot <- list()
  #for (j in Bead_Channels){
  for (i in 1:length(filesToOpen)){
  #NotBeadsPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Ce140Di)) +
  NotBeadsPlot[[i]] <- ggplot(Bead_Plot_Data, aes(x=Time/TimeDiv, y = Bead_Plot_Data[,2])) +
    # Only label 0 and max on X Axis
    scale_x_continuous(breaks=seq(0,maxtime[i],maxtime[i])) +
    # Plot all points
    geom_point(shape=".",alpha=1)+
    # Fill with transparent colour fill using density stats
    # ndensity scales each graph to its own min/max values
    stat_density2d(geom="raster", aes(fill=..ndensity.., alpha = ..ndensity..), contour = FALSE) +
    # Produces a colour scale based on the colours in the colfunc list
    scale_fill_gradientn(colours=colfunc(128)) +
    # And scale y to log, displaying numbers rather than notation
    scale_y_log10(labels = scales::trans_format('log10',scales::math_format(10^.x))) +
    # Force Y axis to start at zero and stop at max signal
    #ylim(0,10000) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none") +
    # Draw gate
    #geom_rect(xmin=0, ymin=0, xmax=maxtime, ymax=NotBeadMax, colour="red", alpha=0)
    geom_hline(yintercept=NotBeadMax[i], colour="red") +
    # cHANGE Y axis label
    ylab(Channel_To_Gate) +
    # Change X axis label
    xlab("Time (min)")+
    ggtitle(paste(round(NotBeadsPop[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep="")) +
    theme(plot.title = element_text(size=8))+
    coord_cartesian(expand = FALSE)
  #}
    # Name each plot with bead channel
    #assign(j, NotBeadsPlot)
  }
  options(warn=0)


  # Gate Not-Beads
  #for (j in Bead_Channels){
    for (i in 1:length(filesToOpen)){
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Ce140Di`)<NotBeadMax[i]]
    #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]][,j]) < NotBeadMax[i]]
      #fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]][,Channel_To_Gate]) < NotBeadMax[i]]
      fcs_raw[[i]] <- fcs_raw[[i]][EventsToKeep[[i]],]
    }
  #}



} # End of beads removal loop

  # Advance progress bar
  setTkProgressBar(pb, 8, label="Creating Plots (this can take time)...")

  # Create new directory to write files into
  options(warn=-1)
  dir.create("CyTOFClean")
  options(warn=0)
  # Set wd to this
  setwd("CyTOFClean")

  # Plots when we aren't gating beads
  if (tclvalue(gbvalue)==0){
  options(warn=-1)
  # Show and save Gaussian plots
  #plot_grid(plotlist = c(EventPlot,CentrePlot,OffsetPlot,ResidualPlot,NotBeadsPlot), nrow = 5)
  plot_grid(plotlist = c(EventPlot,CentrePlot,OffsetPlot,ResidualPlot, WidthPlot), nrow = 5)
  options(warn=0)
  ImgDPI <- 90
  TimeStamp <- gsub(":","_",format(Sys.time(), "%X"))
  ggsave(file = paste("Plots_", TimeStamp, ".png", sep=""),
           width = length(filesToOpen)*200/ImgDPI,
           # height is 180 * number of plots
           height = 180*5/ImgDPI, dpi = ImgDPI, limitsize = FALSE)
  }

  # Plots when we are gating beads
  if (tclvalue(gbvalue)==1){
  options(warn=-1)
  # Show and save Bead plots
  #plot_grid(plotlist = c(EventPlot,CentrePlot,OffsetPlot,ResidualPlot,NotBeadsPlot), nrow = 5)
  #plot_grid(plotlist = eval(parse(text=paste("c(",paste(Bead_Channels, sep="", collapse=", "), ")",sep=""))),
  #          nrow = length(Bead_Channels))
  plot_grid(plotlist = c(EventPlot,CentrePlot,OffsetPlot,ResidualPlot, WidthPlot, NotBeadsPlot), nrow = 6)
  options(warn=0)
  ImgDPI <- 90
  TimeStamp <- gsub(":","_",format(Sys.time(), "%X"))
  ggsave(file = paste("Plots_", TimeStamp, ".png", sep=""),
         width = length(filesToOpen)*200/ImgDPI,
         # height is 150 * number of plots
         height = 150*6/ImgDPI, dpi = ImgDPI, limitsize = FALSE)
}



  # Advance progress bar
  setTkProgressBar(pb, 10, label="Finished Plots ...")


  # Write FCS file(s)
    if (file.exists(paste(gsub(".fcs$|.FCS$","",filesToOpen[1]),"_CC.fcs",sep=""))){
      # Add Timestamps
      for (i in 1:length(filesToOpen)){
        write.FCS(fcs_raw[[i]], filename = paste(gsub(".fcs$|.FCS$","",filesToOpen[i]),"_CC_",TimeStamp,".fcs",sep=""))
      }
    }else{
      for (i in 1:length(filesToOpen)){
        write.FCS(fcs_raw[[i]], filename = paste(gsub(".fcs$|.FCS$","",filesToOpen[i]),"_CC.fcs",sep=""))
      }
    }

  # Get new file sizes
  NewFileSize <- round(sum(file.size(gsub(".fcs$|.FCS$","_CC.fcs",filesToOpen)))/(1024*1024),0)

  # Calculate reduction
  FileSizeReduction <- round((1-(NewFileSize/TotalFileSize))*100,1)

  # Advance progress bar
  setTkProgressBar(pb, 11, label="Done!")

  # End timer
  end_time <- Sys.time()
  Processing_time <- end_time - start_time

  units <- units(Processing_time)

  # Close progress bar
  close(pb)

  # Show summary
  tkmessageBox(title = "CyTOFClean Done!",
               message = paste("Processing Time: ", round(Processing_time,1), units,
               "\nFile size reduced by:", FileSizeReduction,"%",
               "\nFiles are located in:", getwd(),sep=" "), type = "ok")

  # Set working directory back to what it was when we started
  setwd(cur_dir)

  } # End of file check error loop
} # End OK button
}# End of function
