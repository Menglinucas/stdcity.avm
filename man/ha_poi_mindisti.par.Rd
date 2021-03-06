% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ha_poi_mindist.R
\name{ha_poi_mindisti.par}
\alias{ha_poi_mindisti.par}
\title{compute distance between ha and its nearest POI(parallel).}
\usage{
ha_poi_mindisti.par(ha, poi)
}
\arguments{
\item{ha}{SpatialPointDataframe. a ha spatial points data}

\item{poi}{SpatialPointDataframe. a POI spatial points data}
}
\value{
RasterLayer
}
\description{
function which compute distance between ha and its nearest POI.
}
\details{
The unit of distance is meters(great circle distance).This function will utilize parallel package to speedup the computation.
}
