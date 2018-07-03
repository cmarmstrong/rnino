#' SST data
#'
#' Download SST data from <https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/>
#'
nino <- function(index=c('nino1+2', 'nino3', 'nino3.4', 'nino4'), anom=FALSE) {
    lNino <- list('nino1+2'='https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino12.long',
                  'nino3'='https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino3.long',
                  'nino3.4'='https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long',
                  'nino4'='https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino4.long')
    uri <- lNino[index]
    urlNino <- ifelse(anom, paste0(uri, '.anom.data'), paste0(uri, '.data'))
    conn <- url(urlNino)
    dat <- readLines(conn)
    years <- strsplit(trimws(dat[1]), ' +')[[1]]
    n <- years[2]
    dat.1 <- sapply(strsplit(trimws(dat), ' +'), '[', 1)
    n <- which(dat.1==n)
    payload <- dat[2:n]
    mNino <- matrix(unlist(strsplit(trimws(payload), ' +')), nrow=n-1, byrow=TRUE)
    colnames(mNino) <- c('year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    mNino
}
