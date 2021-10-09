# === Prep ----------------------------------------------------------------



# Set up columns ----------------------------------------------------------
prep_cols <- function(DT) {
  DT[, datetime := as.POSIXct(paste(Date, Time, tz = 'UTC', format = '%Y-%m-%d %H:%M'))]
  DT[, x := round(x_cm + 370194, 2)]
  DT[, y := round(y_cm + 5268492, 2)]
  
  old_names <- c('x', 'y', 'datetime', 'Snail', 'Temperature', 
                 'Precipitation', 'Treatment', 'Stage')
  new_names <- c('x', 'y', 't', 'snail', 'temp', 'precip', 'treatment', 'stage')
  DTsubcols <- DT[, .SD, .SDcols = old_names]
  setnames(DTsubcols, old_names, new_names)
  DTsubcols
}



# Make unique and complete ------------------------------------------------
make_unique_complete <- function(DT, id, datetime, long, lat) {
  na.omit(unique(DT, by = c(id, datetime)),
          cols = c(long, lat, datetime))
}

