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
fig_magicc_comparison <- function( d, vtagname, prettylabel ) {
	printlog( "Plotting", vtagname )
	printdims( d )
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

if( exists( "RANDOM_SEED" ) ) {
	printlog( "Setting random generator seed to", RANDOM_SEED )
	set.seed( RANDOM_SEED )
}

loadlibs( c( "ggplot2", "reshape2", "dplyr", "lubridate" ) )	# the hadleyverse
theme_set( theme_bw() )

# ----- Main script goes here...

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
d <- subset( d, !spinup | is.na( spinup ) )
printdims( d )


fig_magicc_comparison( d, "atmos_co2", expression( Atmospheric~CO[2]~(ppmv) ) )


# expression( SR[annual]~(g~C~m^{-2}~yr^{-1}) )
# text( x, y, bquote( R^{2} == .(r2) ) )
# expression( italic(T)[5]~group("(",paste(degree,C),")") )


print( sessionInfo() )
printlog( "All done with", SCRIPTNAME )
sink()
