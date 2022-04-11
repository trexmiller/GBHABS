#' list sites with vertical profile data
#'
#' @keywords sonde data
#' @export
#' @examples vpSites()
#'
vpSites = function(){
  unique(VP$Site_ID)
}

#' list sonde data variables
#'
#' @keywords sonde data
#' @export
#' @import reshape2
#' @examples vpVars()
#'
vpVars = function(){
  colnames(VP)[7:19]
}

#' Get vertical profile data for a sonde parameter
#' @param VP Green Bay vertical profile data frame
#' @param Year A year of the study as character one of "2016" through "2020"
#' @param Site A sampling site as given by function vpSites()
#' @param Var A sonde data variable to get as listed in the function vpVars()
#' @keywords sonde data
#' @export
#' @examples getVP(VP, Year = "2017", Site = 16, Var = Wtemp_C)
#' @returns A data frame of variable data by depth (meters) in rows and date as columns
getVP = function(VP,Year, Site, Var){
  Temp = VP[VP$Site_ID==Site,]
  VarTemp = Temp[Temp$Year==Year,Var]
  Dates=as.POSIXct(Temp[Temp$Year==Year,"Date_Samp"],format = "%m/%d/%Y")
  Depth = Temp[Temp$Year==Year,"DepthEdit_m"]

  TempDepth = data.frame(Dates,Depth,VarTemp)
  TempDepth2 = TempDepth[complete.cases(TempDepth),]
  colnames(TempDepth2) = c("Dates","Depth","Var")
  Out = dcast(TempDepth2,Depth~Dates,value.var = "Var")
  return(Out)
}


#' Find average temperature difference between lake surface and bottom.
#' @param Year A year of the study as character one of "2016" through "2020"
#' @param VP Green Bay vertical profile data frame
#' @param Site A sampling site as given by function vpSites()
#' @keywords sonde data
#' @examples getTDiff(VP, Year = "2017", Site = 16)
#' @export
#' @returns Returns a vector of difference in temperature from surface minus bottom each sampling day at the site indicated. It averages top two depths of 0 and 1 m and two bottom depths then subtracts
getTDiff = function(Year, VP, Site){
  VP = getVP(VP,Year,Var = "Wtemp_C", Site)
  Temp2 = VP[,2:ncol(VP)]
  TempDiff = as.numeric(Temp2[1,])-as.numeric(Temp2[nrow(Temp2),])
  return(TempDiff)
}

#' Get all data for a sonde parameter
#' @param  Var The sonde parameter of interest
#' @param  VPSites The sampling sites of interest as given by vpSites()
#' @keywords sonde data
#' @examples vpList(Var = vpVars()[1])
#' @export
#' @returns Returns a list of data frames, one for each year of the study with the Var chosen by depth and date
vpList = function(Var,VPSites){
  VPList = vector(mode = "list",length = 0)
  for (i in 1:length(VPSites)){
    for(j in 1:length(Years)){
      Temp = getVP(VP,Years[j], VPSites[i],Var)
      VPList[[length(VPList)+1]] = Temp
    }
  }

  NamesVPList = NULL
  for (i in 1:length(VPSites)){
    for(j in 1:length(Years)){
      NamesVPList[length(NamesVPList)+1] = paste("Site ",VPSites[i],Years[j])
    }
  }

  names(VPList) = NamesVPList
  return(VPList)
}

#' Find average sonde data from a given range of depths off the lake bottom
#' @param L A list created from the function vpList
#' @param D Depths from the lake bottom to average
#' @keywords sonde data
#' @export
#' @examples avgBVP(L = vpList(Var = vpVars()[1]), D = 2)
#' @returns Returns a list of vectors with average sonde data variable over the depth range from the bottom up to D meters from the bottom
avgBVP = function(L,D){
  B = vector(mode = "list",length = length(L))
  for (i in 1:length(L)){
    Temp = L[[i]]
    B[[i]] = as.numeric(colMeans(Temp[nrow(Temp):(nrow(Temp)-D),2:ncol(Temp)],na.rm=TRUE))
  }
  return(B)
}

#' Find average sonde data over all depths
#' @param L A list created from the function vpList
#' @keywords sonde data
#' @export
#' @examples avgVP(L = vpList(Var = vpVars()[1], vpSites()[1:3]))
#' @returns Returns a list of vectors with average sonde data of the chosen variable over all depths
avgVP = function(L){
  B = vector(mode = "list",length = length(L))
  for (i in 1:length(L)){
    Temp = L[[i]]
    B[[i]] = as.numeric(colMeans(Temp[,2:ncol(Temp)],na.rm=TRUE))
  }
  return(B)
}
