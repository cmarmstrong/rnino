#' SST data
#'
#' Download SST data from <https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/>
#'
#' Details copied from
#' <https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni>
#'
#' The numbers of the Ni\~{n}o 1,2,3, and 4 regions correspond with the labels
#' assigned to ship tracks that crossed these regions.
#'
#' Ni\~{n}o 1+2 (0-10S, 90W-80W): The Ni\~{n}o 1+2 region is the smallest and
#' eastern-most of the Ni\~{n}o SST regions, and corresponds with the region of
#' coastal South America where El Ni\~{n}o was first recognized by the local
#' populations.  This index tends to have the largest variance of the Ni\~{n}o SST
#' indices.
#' 
#' Ni\~[n}o 3 (5N-5S, 150W-90W): This region was once the primary focus for
#' monitoring and predicting El Ni\~{n}o, but researchers later learned that the
#' key region for coupled ocean-atmosphere interactions for ENSO lies further
#' west (Trenberth, 1997).  Hence, the Ni\~{n}o 3.4 and ONI became favored for
#' defining El Ni\~{n}o and La Ni\~{n}a events.
#' 
#' Ni\~{n}o 3.4 (5N-5S, 170W-120W): The Ni\~{n}o 3.4 anomalies may be thought of
#' as representing the average equatorial SSTs across the Pacific from about the
#' dateline to the South American coast. The Ni\~{n}o 3.4 index typically uses a
#' 5-month running mean, and El Ni\~{n}o or La Ni\~{n}a events are defined when
#' the Ni\~{n}o 3.4 SSTs exceed +/- 0.4C for a period of six months or more.
#' 
#' ONI (5N-5S, 170W-120W): The ONI uses the same region as the Ni\~{n}o 3.4
#' index. The ONI uses a 3-month running mean, and to be classified as a
#' full-fledged El Ni\~{n}o or La Ni\~{n}a, the anomalies must exceed +0.5C or
#' -0.5C for at least five consecutive months. This is the operational
#' definition used by NOAA.
#' 
#' Ni\~{n}o 4 (5N-5S, 160E-150W): The Ni\~{n}o 4 index captures SST anomalies in
#' the central equatorial Pacific. This region tends to have less variance than
#' the other Ni\~{n}o regions.
#'
#' The datasets provide the sea surface temperature (SST) used to compute the
#' nino indices.  The HadISST1 is an interpolated dataset with global and
#' smoothly varying values, provided and maintained by the Met Office Hadley
#' Center.  HadSST3 is the most recent version of the Hadley data, and corrects
#' many biases of the previous data.  It is not (yet) interpolated, but offers
#' multiple realizations of the data that facilitate analysis of uncertainty.
#' 
#' @param index Character.  Indicates the nino index, possible values are
#'   'nino1+2', 'nino3', 'nino3.4', or 'nino4'.  Default 'nino1+2'.
#' @param anom Logical.  Indicates if the index should be anomalies (deviation
#'   from mean) or absolute values.  Only available for HadISST1; default FALSE.
#' @return Dataframe.  Both datasets have a year and month column.  HadISST1
#'   will have a single value column.  HadSST3 will have 100 value columns, each
#'   representing a single realization.  A value of -99 indicates a missing
#'   value.
#' @export
hadisst1 <- function(index=c('nino12', 'nino3', 'nino34', 'nino4'), anom=FALSE) {
    lNino <- list('nino12'='esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino12.long',
                  'nino3'='esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino3.long',
                  'nino34'='esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long',
                  'nino4'='esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino4.long',
                  'oni'='esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino4.long')
    uri <- paste0('https://www.', lNino[[index]])
    urlNino <- ifelse(anom==TRUE, paste0(uri, '.anom.data'), paste0(uri, '.data'))
    conn <- url(urlNino)
    on.exit(close(conn))
    dat <- readLines(conn)
    browser()
    years <- strsplit(trimws(dat[1]), ' +')[[1]]
    n <- years[2]
    dat.1 <- sapply(strsplit(trimws(dat), ' +'), '[', 1)
    n <- which(dat.1==n)
    payload <- dat[2:n]
    mNino <- matrix(unlist(strsplit(trimws(payload), ' +')), nrow=n-1, byrow=TRUE)
    colnames(mNino) <- c('year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
                         'Oct', 'Nov', 'Dec')
    mNino
}

#' @ describeIn nino Download SST data from
#' <https://www.metoffice.gov.uk/hadobs/hadsst3/data/download.html>
#' @export
hadsst3 <- function(index=c('nino12', 'nino3', 'nino34', 'nino4')) {
    lNino <- list('nino12'='metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/ascii/Nino_12.txt',
                  'nino3'='metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/ascii/Nino_3.txt',
                  'nino34'='metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/ascii/Nino_34.txt',
                  'nino4'='metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/ascii/Nino_4.txt')
    urlNino <- paste0('https://www.', lNino[[index]])
    conn <- url(urlNino)
    dfrNino <- read.table(conn)
    names(dfrNino) <- c('year', 'month', 1:100)
    dfrNino
}
