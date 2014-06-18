# R script to generate figures for documentation ms
# Ben Bond-Lamberty & Corinne Hartin
# June 2014

# Support functions and common definitions

INPUT_FILE		<- "figuredata.csv"
SCRIPTNAME		<- "figures.R"
OUTPUT_DIR		<- "outputs/"
LOG_DIR			<- "logs/"
SEPARATOR		<- "-------------------"

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), " " )
	cat( msg, ... )
	if( cr ) cat( "\n")
} # printlog

# -----------------------------------------------------------------------------
# Print dimensions of data frame
printdims <- function( d, dname=deparse( substitute( d ) ) ) {
	stopifnot( is.data.frame( d ) )
	printlog( dname, "rows =", nrow( d ), "cols =", ncol( d ) )
} # printdims

# -----------------------------------------------------------------------------
# Save a ggplot figure
saveplot <- function( pname, p=last_plot(), ptype=".pdf" ) {
	stopifnot( file.exists( OUTPUT_DIR ) )
	fn <- paste0( OUTPUT_DIR, "/", pname, ptype )
	printlog( "Saving", fn )
	ggsave( fn, p )
} # saveplot

# -----------------------------------------------------------------------------
# Save a data frame
savedata <- function( df, extension=".csv" ) {
	stopifnot( file.exists( OUTPUT_DIR ) )
	fn <- paste0( OUTPUT_DIR, "/", deparse( substitute( df ) ), extension )
	printlog( "Saving", fn )
	write.csv( df, fn, row.names=F )
} # saveplot

# -----------------------------------------------------------------------------
# Open a csv file and return data
read_csv <- function( fn, datadir=".", ... ) {
	fqfn <- paste( normalizePath( datadir ), fn, sep="/" )
	printlog( "Opening", fqfn )
	stopifnot( file.exists( fqfn ) )
	read.csv( fqfn, stringsAsFactors=F, ... )
} # read_csv

# -----------------------------------------------------------------------------
# Load requested libraries
loadlibs <- function( liblist ) {
	printlog( "Loading libraries..." )
	loadedlibs <- vector()
	for( lib in liblist ) {
		printlog( "- loading", lib )
		loadedlibs[ lib ] <- require( lib, character.only=T )
		if( !loadedlibs[ lib ] )
			warning( "this package is not installed!" )
	}
	invisible( loadedlibs )
} # loadlibs

# ==============================================================================
# Figure functions

# -----------------------------------------------------------------------------
# Change to relative values, adjusting the error terms
make_relative <- function( d ) {

	dph_s <- d[ order( d$year ), ]	#sorts in order of year 

	dph_s$error_min <- with( dph_s, error_min - value )
	dph_s$error_max <- with( dph_s, error_max - value )
	d_norm <- ddply( dph_s, .( ctag, vtag, model, type, units, scenario), mutate, 
		value = value - value[1],  
		error_min = error_min,
		error_max = error_max )
	dph_s$error_min <- with( dph_s, error_min + value )
	dph_s$error_max <- with( dph_s, error_max + value )
	dph_s
}

# -----------------------------------------------------------------------------
fig_timeseries <- function( d, vtagname, prettylabel ) {

	d1 <- subset( d1, vtag==vtagname &
		model %in% c( "CMIP5", "HECTOR", "MAGICC 5.3", "MAGICC6" )) #, "GCP", "BATS", "HOTS"))
	
}

# -----------------------------------------------------------------------------
fig_datasummary <- function( d ) {
	# Make a summary plot showing what variables are available to plot
	dgrp <- group_by( d, scenario, type, model )	# dplyr
	dsum <- na.omit( summarise( dgrp, count=n() ) )
	dsum$count_grp <- cut(dsum$count,c(1,10,100,1000,10000,100000))

	dev.new( height=10, width=10 )
	p <- qplot( scenario, model, data=subset( dsum, type!="observed" ), geom="tile", fill=count_grp )
	print( p )
	saveplot( "summary_scenario_model" )

	dgrp <- group_by( d, vtag, type, model )	# dplyr
	dsum <- na.omit( summarise( dgrp, count=n() ) )
	dsum$count_grp <- cut( dsum$count,c( 1, 10,100,1000,3000))
	p <- qplot( vtag, model, data=subset( dsum, type!="observed" ), geom="tile", fill=count_grp )
	p <- p +  theme( axis.text.x  = element_text( angle=90 ) )
	print( p )
	saveplot( "summary_vtag_model" )
	dev.off()

}

# -----------------------------------------------------------------------------
fig_taylor <- function( d, vtagname, prettylabel ) {
	printlog( "Plotting", vtagname )
	printdims( d )
	d1 <- subset( d, vtag==vtagname )

}

# -----------------------------------------------------------------------------
fig_magicc_comparison <- function( d, vtagname, prettylabel ) {
	printlog( "Plotting", vtagname )
	d1 <- subset( d, vtag==vtagname & model %in% c( "HECTOR", "MAGICC6" ) )
	printdims( d1 )
	p <- ggplot( d1, aes( year, value, color=scenario ) ) 
	p <- p + geom_line( aes( color=scenario, linetype=model ) )
	p <- p + xlab( "Year" ) + ylab( prettylabel )
	p <- p + scale_color_discrete( "RCP", labels=c("2.6","4.5","6.0","8.5") )
	p <- p + scale_linetype_discrete( "Model", labels=c( "Hector", "MAGICC" ) )
	p <- p + xlim( c( 1850, 2300 ) )
	print( p )
	saveplot( paste0( "magicc_comparison_", vtagname ) )
	invisible( p )
}


# ==============================================================================
# Main

OUTPUT_DIR <- normalizePath( OUTPUT_DIR )
if( !file.exists( OUTPUT_DIR ) ) {
	printlog( "Creating", OUTPUT_DIR )
	dir.create( OUTPUT_DIR )
}
LOG_DIR <- normalizePath( LOG_DIR )
if( !file.exists( LOG_DIR ) ) {
	printlog( "Creating", LOG_DIR )
	dir.create( LOG_DIR )
}

sink( paste( LOG_DIR, paste0( SCRIPTNAME, ".txt" ), sep="/" ), split=T )

printlog( "Welcome to", SCRIPTNAME )

loadlibs( c( "ggplot2", "reshape2", "dplyr", "lubridate", "plotrix" ) )	# the hadleyverse
theme_set( theme_bw() )

# R reads big files a LOT faster if you pre-specify the column types
cc <- c( 	"ctag"="factor",
			"variable"="factor",
			"component"="factor",
			"year"="numeric",
			"run_name"="factor",
			"spinup"="numeric",
			"value"="numeric",
			"units"="factor",
			"source"="factor",
			"scenario"="character",
			"model"="character",
			"type"="factor",
			"error"="numeric", "error_min"="numeric", "error_max"="numeric",
			"old_ctags"=NULL, "notes.x"=NULL ,
			"vtag"="character",
			"prettylabel"=NULL, "old_vtags"=NULL, "notes.y"=NULL,"variable.x"=NULL,"variable.y"=NULL )
d <- read_csv( INPUT_FILE, colClasses=cc  )
printdims( d )
d <- subset( d, !spinup | is.na( spinup ) )		# remove all spinup data
d$spinup <- NULL
printdims( d )

# Taylor plots comparing Hector with CMIP5 models


# Time series plots with CMIP5 and MAGICC



# Figures comparing Hector and MAGICC (no CMIP)
printlog( "Figures comparing Hector and MAGICC (no CMIP)" )
fig_magicc_comparison( d, "atmos_co2", expression( Atmospheric~CO[2]~(ppmv) ) )
fig_magicc_comparison( d, "tgav", expression( Temperature~group("(",paste(degree,C),")") ) )
fig_magicc_comparison( d, "ftot", expression( Forcing~(W~m^{-2}) ) )



print( sessionInfo() )
printlog( "All done with", SCRIPTNAME )
sink()
