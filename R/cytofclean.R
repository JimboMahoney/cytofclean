### Auto-cleanup of CyTOF FCS
### Credit for GUI code: https://github.com/JinmiaoChenLab/cytofkit

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

## submit / reset / quit
submit_button <- tkbutton(tt, text = "Process Files...", command = submit)
quit_button <- tkbutton(tt, text = "Quit", command = quit)

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

tkgrid(tklabel(tt, text = ""),tklabel(tt, text = "NOTE: Only Helios / CyTOF v3 files are currently supported!"))
tkgrid(tklabel(tt, text = ""),tklabel(tt, text = "Files should be normalised before using CyTOFClean."))


tkgrid(tklabel(tt, text = "\n"), padx = cell_width)  # leave blank line

### Go and Quit buttons
tkgrid(tklabel(tt, text = ""), submit_button,
       quit_button, padx = cell_width)
tkgrid.configure(quit_button, sticky = "w")
tkgrid(tklabel(tt, text = ""),tklabel(tt, text = ""),tklabel(tt, text = "Version: 0.1 beta"))

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
                      max = 10, width = 300)

  # Advance progress bar
  setTkProgressBar(pb, 1, label="Loading Files ...")


  # Read the files into a flowset
  fcs_raw <- read.flowSet(filesToOpen, transformation = FALSE,
                          truncate_max_range = FALSE)


  # Get parameters
  params <- pData(parameters(fcs_raw[[1]]))

  # Check for Event_length & 140 Channel
  if ("Event_length" %in% params$name == FALSE){
    tkmessageBox(title = "Error!",
                 message = "Only Helios / CyTOF v3 files are supported!", type = "ok")
    # Close progress bar
    close(pb)
  }

  if ("Ce140Di" %in% params$name == FALSE){
    tkmessageBox(title = "Error!",
                 message = "Missing EQ Bead (Ce140) Channel!", type = "ok")
    # Close progress bar
    close(pb)
  }else{




  # Advance progress bar
  setTkProgressBar(pb, 2, label="Gating Events ...")

  # Get original cell number
  OrigEvents <- NULL
  for (i in 1:length(filesToOpen)){
    OrigEvents[i] <- length(exprs(fcs_raw[[i]]$`Event_length`))
  }

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
  maxtime <- round(maxtime/TimeDiv,1)

  # Create colour scale for plot
  # The extra "black" is needed to make the colour scale appear decent - otherwise it doesn't show
  colfunc <- colorRampPalette(c("black",# "black","black", "black", "black",# "black", "black", "black",
                                "purple4", #"purple4",
                                "red", "yellow"))

  # Function to find peaks - too risky with some datasets?
  modes <- function(d){
    i <- which(diff(sign(diff(d$y))) < 0) + 1
    data.frame(x = d$x[i], y = d$y[i])
  }
  # Function to find troughs - too risky with some datasets?
  #localmin <- function(d){
  #  i <- which(diff(sign(diff(d$y))) > 0) + 1
  #  data.frame(x = d$x[i], y = d$y[i])
  #}

  # Event stats
  EventSD <- NULL
  EventMed <- NULL
  #EventMean <- NULL
  for (i in 1:length(filesToOpen)){
    EventSD[i] <- sd(exprs(fcs_raw[[i]]$`Event_length`))
    EventMed[i] <- median(exprs(fcs_raw[[i]]$`Event_length`))
  #  EventMean[i] <- mean(exprs(fcs_raw[[i]]$`Event_length`))
  }

  # Find Mode / Peak Density of Events - ignoring any below 10 and above 40
  EventModes <- list()
  EventMode <- NULL
  for (i in 1:length(filesToOpen)){
    EventModes[[i]] <-  modes(density(exprs(fcs_raw[[i]]$`Event_length`), adjust=1))
    # Remove any non-sensical values
    EventModes[[i]] <- EventModes[[i]][EventModes[[i]][,"x"]>10 & EventModes[[i]][,"x"]<40,]
    EventMode[i] <- EventModes[[i]][,"x"][EventModes[[i]][,"y"]==max(EventModes[[i]][,"y"])]
  }

  # Find localmins - using "smoothing" value of 1 - should find first minimum after peak
  #EventLocalMins <- list()
  #EventMax <- NULL
  #for (i in 1:length(filesToOpen)){
  #  EventLocalMins[[i]] <-  localmin(density(exprs(fcs_raw[[i]]$`Event_length`), adjust=3))
  #  EventMax[i] <- EventLocalMins[[i]][,"x"][EventLocalMins[[i]][,"x"]>0][EventLocalMins[[i]][,"y"]==max(EventLocalMins[[i]][,"y"])]
  #}

  EventBias <- NULL
  for (i in 1:length(filesToOpen)){
    EventBias[i] <- EventMed[i] / EventMode[i]
  }

  # Set Min and Max
  EventMin <- NULL
  EventMax <- NULL
  for (i in 1:length(filesToOpen)){
    EventMin[i] <- EventMode[i] - EventSD[i] * 0.6 * 1/EventBias[i]
    EventMax[i] <- EventMode[i] + EventSD[i] * 0.85 * EventBias[i]
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
    # Force Y axis to start at zero and stop at 150
    ylim(0,50) +
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
    theme(plot.title = element_text(size=8, hjust=0.5), plot.subtitle = element_text(size=8))
  }
  options(warn=0)


  #Gate Events
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Event_length`)<EventMax[i]
                                 & exprs(fcs_raw[[i]]$`Event_length`)>EventMin[i]]
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

  # Centre stats
  CentreSD <- NULL
  CentreMed <- NULL
  #CentreMean <- NULL
  for (i in 1:length(filesToOpen)){
    CentreSD[i] <- sd(exprs(fcs_raw[[i]]$`Center`))
    CentreMed[i] <- median(exprs(fcs_raw[[i]]$`Center`))
  #  CentreMean[i] <- mean(exprs(fcs_raw[[i]]$`Center`))
  }

  CenterModes <- list()
  CentreMode <- NULL
  for (i in 1:length(filesToOpen)){
    CenterModes[[i]] <-  modes(density(exprs(fcs_raw[[i]]$`Center`), adjust=3))
    # Remove any negative values
    CenterModes[[i]] <- CenterModes[[i]][CenterModes[[i]][,"x"]>0,]
    CentreMode[i] <- CenterModes[[i]][,"x"][CenterModes[[i]][,"y"]==max(CenterModes[[i]][,"y"])]
  }

  CentreBias <- NULL
  CentreMin <- NULL
  CentreMax <- NULL
  for (i in 1:length(filesToOpen)){
    CentreBias[i] <- CentreMed[i] / CentreMode[i]
    #CentreMin[i] <- CentreMed[i] -  2.4 * CentreSD[i] * 0.7 / CentreBias[i]
    #CentreMax[i] <- CentreMed[i] +  2.4 * CentreSD[i] * CentreBias[i]
    CentreMin[i] <- CentreMode[i] -   1.7 * CentreSD[i] * 1/CentreBias[i]^1.6
    CentreMax[i] <- CentreMode[i] +  1.7 * CentreSD[i] * CentreBias[i]^1.6
  }

  # Clip any min to 250 - this helps with very "dirty" samples
  CentreMin[CentreMin<250] <- 250


  # % Cells After Centre gating
  AfterCentre <- NULL
  for (i in 1:length(filesToOpen)){
    AfterCentre[i] <- sum(exprs(fcs_raw[[i]]$`Center`)<CentreMax[i]
                          & exprs(fcs_raw[[i]]$`Center`)>CentreMin[i])/length(exprs(fcs_raw[[i]]$`Center`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterCentre[i]

    FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
  }

  # Get Mean Mode and SD value for Y axis size
  CentreYAxis <- mean(CentreMode) / mean(CentreSD) * 1000

  options(warn=-1)
  ## Plot x as Time and Y as Center
  CentrePlot <- list()
  for (i in 1:length(filesToOpen)){
  CentrePlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Center)) +
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
    theme(plot.title = element_text(size=8))
  }
  options(warn=0)

  # Gate Centre
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Center`)<CentreMax[i] & exprs(fcs_raw[[i]]$`Center`)>CentreMin[i]]
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


  # Offset stats
  OffsetSD <- NULL
  OffsetMed <- NULL
  #OffsetMean <- NULL
  for (i in 1:length(filesToOpen)){
    OffsetSD[i] <- sd(exprs(fcs_raw[[i]]$`Offset`))
    OffsetMed[i] <- median(exprs(fcs_raw[[i]]$`Offset`))
    #OffsetMean[i] <- mean(exprs(fcs_raw[[i]]$`Offset`))
  }


  OffsetModes <- list()
  OffsetMode <- NULL
  for (i in 1:length(filesToOpen)){
    OffsetModes[[i]] <-  modes(density(exprs(fcs_raw[[i]]$`Offset`), adjust=3))
    # Remove any negative values
    OffsetModes[[i]] <- OffsetModes[[i]][OffsetModes[[i]][,"x"]>0,]
    OffsetMode[i] <- OffsetModes[[i]][,"x"][OffsetModes[[i]][,"y"]==max(OffsetModes[[i]][,"y"])]
  }


  OffsetMin <- NULL
  OffsetMax <- NULL
  OffsetBias <- NULL
  for (i in 1:length(filesToOpen)){
    OffsetBias[i] <- OffsetMed[i] / OffsetMode[i]
    OffsetMin[i] <- OffsetMode[i] - 1.1 * OffsetSD[i] * 1/OffsetBias[i]
    OffsetMax[i] <- OffsetMode[i] + 1.1 * OffsetSD[i] * OffsetBias[i]
  }

  # Clip any min to zero
  OffsetMin[OffsetMin<0] <- 0

  # % Cells After Offset gating
  AfterOffset <- NULL
  for (i in 1:length(filesToOpen)){
    AfterOffset[i] <- sum(exprs(fcs_raw[[i]]$`Offset`)<OffsetMax[i]
                          & exprs(fcs_raw[[i]]$`Offset`)>OffsetMin[i])/length(exprs(fcs_raw[[i]]$`Offset`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterOffset[i]

    FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
  }

  # Get Mean Mode value for Y axis size
  OffsetYAxis <- mean(OffsetMode) * 4

  options(warn=-1)
  ## Plot x as Time and Y as Offset
  OffsetPlot <- list()
  for (i in 1:length(filesToOpen)){
  OffsetPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Offset)) +
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
    theme(plot.title = element_text(size=8))
  }
  options(warn=0)

  # Gate Offset
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Offset`)<OffsetMax[i] & exprs(fcs_raw[[i]]$`Offset`)>OffsetMin[i]]
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


  # Residual stats
  ResidualSD <- NULL
  ResidualMed <- NULL
  #ResidualMean <- NULL
  for (i in 1:length(filesToOpen)){
    ResidualSD[i] <- sd(exprs(fcs_raw[[i]]$`Residual`))
    ResidualMed[i] <- median(exprs(fcs_raw[[i]]$`Residual`))
    #ResidualMean[i] <- mean(exprs(fcs_raw[[i]]$`Residual`))
  }


  ResidualModes <- list()
  ResidualMode <- NULL
  for (i in 1:length(filesToOpen)){
    ResidualModes[[i]] <-  modes(density(exprs(fcs_raw[[i]]$`Residual`), adjust=3))
    # Remove any negative values
    ResidualModes[[i]] <- ResidualModes[[i]][ResidualModes[[i]][,"x"]>0,]
    ResidualMode[i] <- ResidualModes[[i]][,"x"][ResidualModes[[i]][,"y"]==max(ResidualModes[[i]][,"y"])]
  }


  ResidualBias <- NULL
  ResidualMin <- NULL
  ResidualMax <- NULL
  for (i in 1:length(filesToOpen)){
    ResidualBias[i] <- ResidualMed[i] / ResidualMode[i]
    ResidualMin[i] <- ResidualMode[i] * ResidualBias[i]^1.3 -  1.6 * ResidualSD[i]
    ResidualMax[i] <- ResidualMode[i] +  1.6 * ResidualSD[i] * ResidualBias[i]^0.5
  }

  # Clip any min to zero
  ResidualMin[ResidualMin<0] <- 0

  # % Cells After Residual gating
  AfterResidual <- NULL
  for (i in 1:length(filesToOpen)){
    AfterResidual[i] <- sum(exprs(fcs_raw[[i]]$`Residual`)<ResidualMax[i]
                            & exprs(fcs_raw[[i]]$`Residual`)>ResidualMin[i])/length(exprs(fcs_raw[[i]]$`Residual`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * AfterResidual[i]

    FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
  }

  # Get Mode for Y axis
  ResidualYAxis <- mean(ResidualMode) * 4

  options(warn=-1)
  ## Plot x as Time and Y as Residual
  ResidualPlot <- list()
  for (i in 1:length(filesToOpen)){
  ResidualPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Residual)) +
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
    theme(plot.title = element_text(size=8))
  }
  options(warn=0)

  # Gate Residual
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Residual`)<ResidualMax[i]
                                 & exprs(fcs_raw[[i]]$`Residual`)>ResidualMin[i]]
  }

  # Advance progress bar
  setTkProgressBar(pb, 6, label="Gating Cells ...")

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


  # Not-Bead stats
  #NotBeadSD <- NULL
  #NotBeadMed <- NULL
  NotBeadMean <- NULL
  for (i in 1:length(filesToOpen)){
    #NotBeadSD[i] <- sd(exprs(fcs_raw[[i]]$`Ce140Di`))
    #NotBeadMed[i] <- median(exprs(fcs_raw[[i]]$`Ce140Di`))
    NotBeadMean[i] <- mean(exprs(fcs_raw[[i]]$`Ce140Di`))
    #NotBeadMax <- max(exprs(fcs_raw[[1]]$`Ce140Di`))
  }

  #NotBeadModes <-  modes(density(exprs(fcs_raw[[1]]$`Ce140Di`)))
  #NotBeadMode <- NotBeadModes$x[NotBeadModes$y==max(NotBeadModes$y)]
  NotBeadMax <- NULL
  NotBeadsPop <- NULL
  for (i in 1:length(filesToOpen)){
    #NotBeadMax[i] <- NotBeadMean[i]
    NotBeadMax[i] <- 100
    # % Cells After Not-Beads gating
    NotBeadsPop[i] <- sum(exprs(fcs_raw[[i]]$`Ce140Di`)<NotBeadMax[i])/length(exprs(fcs_raw[[i]]$`Ce140Di`))
  }

  # Get % of original
  FinalEvents <- NULL
  for (i in 1:length(filesToOpen)){
    FinalEvents[i] <- length(exprs(fcs_raw[[i]]$`Time`)) * NotBeadsPop[i]

    FinalEvents[i] <- FinalEvents[i]/OrigEvents[i]
  }

  options(warn=-1)
  ## Plot x as Time and Y as 140
  NotBeadsPlot <- list()
  for (i in 1:length(filesToOpen)){
  NotBeadsPlot[[i]] <- ggplot(as.data.frame(exprs(fcs_raw[[i]][RandEvents[[i]],])), aes(x=Time/TimeDiv, y=Ce140Di)) +
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
    scale_y_log10(labels=scales::trans_format('log10',scales::math_format(10^.x))) +
    # Force Y axis to start at zero and stop at maxEvent
    #ylim(0,NotBeadMax) +
    # Hide Y axis values if desired
    #theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    # Hide legend
    theme(legend.position = "none") +
    # Draw gate
    #geom_rect(xmin=0, ymin=0, xmax=maxtime, ymax=NotBeadMax, colour="red", alpha=0)
    geom_hline(yintercept=NotBeadMax[i], colour="red") +
  # Hide Y axis label
  #ylab(NULL) +
  # Change X axis label
  xlab("Time (min)")+
  ggtitle(paste(round(NotBeadsPop[i]*100,1)," % (", round(FinalEvents[i]*100,1), " % of total)",sep="")) +
    theme(plot.title = element_text(size=8))
  }
  options(warn=0)

  # Gate Not-Beads
  for (i in 1:length(filesToOpen)){
    fcs_raw[[i]] <- fcs_raw[[i]][exprs(fcs_raw[[i]]$`Ce140Di`)<NotBeadMax[i]]
  }

  # Advance progress bar
  setTkProgressBar(pb, 7, label="Creating Plots (this can take time)...")

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

  # Create new directory to write files into
  options(warn=-1)
  dir.create("CyTOFClean")
  options(warn=0)
  # Set wd to this
  setwd("CyTOFClean")

  options(warn=-1)
  # Show and save plot
  plot_grid(plotlist = c(EventPlot,CentrePlot,OffsetPlot,ResidualPlot,NotBeadsPlot), nrow = 5)
  options(warn=0)
  ImgDPI <- 90
  TimeStamp <- gsub(":","_",format(Sys.time(), "%X"))
  ggsave(file = paste("plots_", TimeStamp, ".png", sep=""),
           width = length(filesToOpen)*200/ImgDPI,
           height = 900/ImgDPI, dpi = ImgDPI,limitsize = FALSE)

  # Advance progress bar
  setTkProgressBar(pb, 9, label="Finished Plots ...")



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
  setTkProgressBar(pb, 10, label="Done!")

  # End timer
  end_time <- Sys.time()
  Processing_time <- end_time - start_time

  units <- units(Processing_time)

  # Close progress bar
  close(pb)

  # Show summary
  tkmessageBox(title = "CyTOFClean Done!",
               message = paste("Processing Time: ", round(Processing_time,0), units,
               "\nFile size reduced by:", FileSizeReduction,"%",
               "\nFiles are located in:", getwd(),sep=" "), type = "ok")

  # Set working directory back to what it was when we started
  setwd(cur_dir)

  } # End of file check error loop
} # End OK button
}# End of function



