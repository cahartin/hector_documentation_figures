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
taylorcolor <- function( modelname ) {
	if( modelname=="CMIP5" ) return( "red" )
	else if( modelname=="MAGICC6" ) return( "blue" )
	else if( modelname=="HECTOR" ) return( "green" )
	else return( "darkgrey" )
}

# -----------------------------------------------------------------------------
fig_taylor <- function( d, vtagname, prettylabel, normalizeYears=NA, refmodel="HECTOR" ) {
    printlog( "Plotting", vtagname )
    d1 <- subset( d, vtag==vtagname )
    d1$future <- d1$year > 2004
    printdims( d1 )
    
    d1 <- renormalize( d1, normalizeYears )
    
    printlog( "Taylor plot..." )		# following code from taylor.diagram help
    library( plotrix )
    pdf( paste0( OUTPUT_DIR, "/", "taylor_", vtagname, ".pdf" ) )
    
    d_ref <- subset( d1, model==refmodel )[ , c( "model", "year", "value" ) ]
    d_others <- subset( d1, model!=refmodel )[ , c( "model", "year", "value" ) ]
    models <- unique( d_others$model )
#    colors <- rainbow( length( models )+1 )
    oldpar <- taylor.diagram( d_ref$value, d_ref$value, main="", 
    						col=taylorcolor( refmodel ),
                            xlab=prettylabel, ylab=prettylabel ) #colors[ 1 ] )
    for( m in 1:length( models ) ) {
        printlog( m, models[ m ] )
        d_mod <- subset( d_others, model==models[ m ] )
        d_mod$year <- round( d_mod$year )   # some models report 2001.5, 2002.5, etc.
        d_plot <- merge( d_ref, d_mod, by="year" )

#        if( nrow( d_mod ) < 3 ) next
        taylor.diagram( d_plot$value.x, d_plot$value.y, add=T, col=taylorcolor( models[ m ] )
    }
    
    # get approximate legend position
    lpos <- 1.35 * sd( d_ref$value )
    # add a legend
    modellist <- c("HECTOR", "MAGICC6", "CMIP5 mean", "CMIP5 model" )
    legend( lpos, lpos, legend=modellist, pch=19, col=taylorcolor( modellist ) )
    # now restore par values
    par( oldpar )
    dev.off()
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

# -----------------------------------------------------------------------------
renormalize <- function( d, normalizeYears ) {
    if( all( !is.na( normalizeYears ) ) ) {
        stopifnot( is.numeric( normalizeYears ) )
        
        # Compute mean value during the normalization period
        # NOTE: 'scenario' isn't included in the ddply call below, because we want
        # RCPs to be normalized by a historical period. But if you're plotting
        # multiple scenarios in a single plot, and normalizing by a future period, 
        # this will fail.
        d_norm <- ddply( d[ d$year %in% normalizeYears, ], 
                         .( ctag, vtag, model ), summarise, 
                         value_norm_mean = mean( value ) )
        d <- merge( d, d_norm )

        # Relativize the error_min and max, normalize, then recompute them
        d$error_min <- with( d, error_min - value )
        d$error_max <- with( d, error_max - value )
        d <- ddply( d, .( ctag, vtag, model, type, units, scenario), mutate, value = value - value_norm_mean,  
                     error_min = error_min , error_max = error_max )
        d$error_min <- with( d, error_min + value )
        d$error_max <- with( d, error_max + value )
        d$value_norm_mean <- NULL
    }
    d
}

# -----------------------------------------------------------------------------
cmip5_comparison <- function( d, vtagname, prettylabel, normalizeYears=NA ) {
    printlog( "Plotting", vtagname )
    d1 <- subset( d, vtag==vtagname )
    d1 <- renormalize( d1, normalizeYears )
    d1$future <- d1$year > 2004
    printdims( d1 )
        
    p <- ggplot( d1, aes( year, value, color=model ) )
    p <- p + geom_line( size=2 )
    p <- p + geom_ribbon( aes( ymin=value-error, ymax=value+error, fill=model ), color=NA, alpha=0.5, show_guide=F )
    p <- p + geom_ribbon( aes( ymin=error_min, ymax=error_max, fill=model ), color=NA, alpha=0.25, show_guide=F )
    p <- p + facet_grid( ~future, scales="free_x", space="free_x" )
    p <- p + theme( strip.text.x = element_blank() ) + theme( strip.background = element_blank() )
    p <- p + xlab( "Year" ) + ylab( prettylabel )
    p <- p + scale_color_discrete( "Model" )
    p <- p + scale_x_continuous( expand=c( 0, 0 ) )
    print( p )
    saveplot( paste0( "cmip5_comparison_", vtagname ) )
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

loadlibs( c( "ggplot2", "reshape2", "plyr", "lubridate", "plotrix", "data.table" ) )	# the hadleyverse
theme_set( theme_bw() )

#theme_update( ... )

# R reads big files a LOT faster if you pre-specify the column types
#cc <- c( 	"ctag"="character",
#			"variable"="character",
#			"component"="character",
#			"year"="numeric",
#			"run_name"="character",
#			"spinup"="numeric",
#			"value"="numeric",
#			"units"="character",
#			"source"="character",
#			"scenario"="character",
#			"model"="character",
#			"type"="character",
#			"error"="numeric", "error_min"="numeric", "error_max"="numeric",
#			"old_ctags"=NULL, "notes.x"=NULL ,
#			"vtag"="character",
#			"prettylabel"=NULL, "old_vtags"=NULL, "notes.y"=NULL,"variable.x"=NULL,"variable.y"=NULL )
#d <- read_csv( INPUT_FILE, colClasses=cc  )
d <- as.data.table( fread( INPUT_FILE ) )  # now using data.table's fread
printdims( d )
d <- subset( d, !spinup | is.na( spinup ) )		# remove all spinup data
d$spinup <- NULL    # ...and a lot of crap fields
d$notes.x <- NULL
d$notes.y <- NULL
d$variable.x <- NULL
d$variable.y <- NULL
d$old_ctags <- NULL
d$old_vtags <- NULL
printdims( d )


# Definitions for 'pretty' axis labels
tgav_pretty <- expression( Temperature~change~group("(",paste(degree,C),")") )
oaflux_pretty <- expression( Atmosphere-ocean~flux~(Pg~C~yr^{-1}))

# Figures comparing Hector and MAGICC (no CMIP)
printlog( "Figures comparing Hector and MAGICC (no CMIP)" )
fig_magicc_comparison( d, "atmos_co2", expression( Atmospheric~CO[2]~(ppmv) ) )
fig_magicc_comparison( d, "tgav", tgav_pretty )
fig_magicc_comparison( d, "ftot", expression( Forcing~(W~m^{-2}) ) )

# Figures comparing Hector and CMIP5 (plus MAGICC)
cmip5 <- subset( d, scenario %in% c( "rcp85", "historical" ) &
                     model %in% c( "HECTOR", "MAGICC6", "CMIP5" ) &
                     year >= 1850 )
cmip5_comparison( cmip5, "tgav", tgav_pretty, normalizeYears=1961:1990 )
cmip5_comparison( cmip5, "atm_ocean_flux", oaflux_pretty, normalizeYears=1850 )

# Taylor plots comparing Hector to all CMIP5 models
cmip5indiv <- subset( d, scenario %in% c( "rcp85", "historical" ) &
                          year >= 1850 )
fig_taylor( cmip5indiv, "tgav", tgav_pretty, normalizeYears=1961:1990 )
fig_taylor( cmip5indiv, "atm_ocean_flux", oaflux_pretty, normalizeYears=1850 )

print( sessionInfo() )
printlog( "All done with", SCRIPTNAME )
sink()
