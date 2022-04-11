#' Get water chemistry data for a given variable from core sampling sites for plotting purposes
#' @param s A core sampling site, run names(ChemPrime)
#' @param v A chemistry var or variable to get, see ChemVars
#' @keywords chemistry data
#' @export
#' @import zoo
#' @examples getChem(s = "GBBB", Year = "2017", Site = 16, Var = "Wtemp_C")
#' @returns A list of 2- column dataframes for each year at one site containing sample dates and var concentrations
getChem = function(s,v){
  Dates = as.POSIXct(ChemPrime[[s]]$Date,format = "%m/%d/%y")
  Var = ChemPrime[[s]][[v]]
  DF = data.frame(Dates,Var)

  Years = c("2016","2017","2018","2019","2020")

  DFL = vector(mode = "list", length = length(Years))
  for (i in 1:length(Years)){
    DFL[[i]] = DF[format(DF$Dates,"%Y")==Years[i],]
  }

  names(DFL)  = paste0("Y",Years)

  PlotData = vector(mode = "list", length = length(Years))
  for (i in 1:length(Years)){
    PlotDF = data.frame(DFL[[paste0("Y",Years[i])]]$Dates, DFL[[paste0("Y",Years[i])]]$Var)
    colnames(PlotDF) = c("Date","Var")
    PlotDF2 = PlotDF[order(PlotDF$Date),]
    Var2 = na.approx(PlotDF2$Var,na.rm=FALSE)
    PlotDF3 = data.frame(PlotDF2$Date,Var2)
    colnames(PlotDF3) = c("Date","Var")
    PlotData[[i]] = PlotDF3
  }
  names(PlotData) = paste0("Y",Years)
  return(PlotData)
}

#' Aggregate chemistry data variable by week of the year in order to compare across sites
#' @param x An object from getChem()
#' @keywords chemistry data
#' @export
#' @import zoo
#' @examples agrWeek(getChem(s = "GBBB", Year = "2017", Site = 16, Var = "Wtemp_C"))
#' @returns A list of 2 column dataframes for each year at a core site containing week of year vs average weekly concentrations
agrWeek = function(x){
  WeekAg = vector(mode = "list", length = length(x))
  for (i in 1:length(x)){
    Weeks = format(x[[i]]$Date,"%V")
    Temp = aggregate(x[[i]]$Var,by=list(as.numeric(Weeks)),FUN = meanNA)
    colnames(Temp) = c("WeekNum","Var")
    WeekAg[[i]] = Temp
  }
  names(WeekAg) = paste0("Y",Years)
  return(WeekAg)
}


#' Get the average, standard deviation and error of chemical variable across sites by week in each year of the Green Bay HABs study
#' @param Sites Which sites to include in the average, from names(ChemPrime) - character vector
#' @param Var Which variable to average, from ChemVars - single character name
#' @param Years Which years to average, from Years object - character vector
#' @keywords chemistry data
#' @export
#' @import zoo
#' @examples getChemAvg(Sites = names(ChemPrime),Var = ChemVars[11])
#' @returns A list of four column dataframes for each year with average weekly concentrations across sampling sites
getChemAvg = function(Sites,Var,Years){
  Out1=vector(mode = "list", length = length(Years))
  for (i in 1:length(Sites)){
    Out1[[i]] = agrWeek(getChem(s = Sites[i], v = Var))
  }
  names(Out1) = Sites

  Out2=vector(mode = "list", length = length(Years))
  for (i in 1:length(Years)){
    Weeks=vector(mode = "list", length = length(Sites))
    for (j in 1:length(Sites)){
      Weeks[[j]] = Out1[[Sites[j]]][[paste0("Y",Years[i])]][,1]
    }
    Weeks = unlist(Weeks)

    Vars=vector(mode = "list", length = length(Sites))
    for (j in 1:length(Sites)){
      Vars[[j]] = Out1[[Sites[j]]][[paste0("Y",Years[i])]][,2]
    }
    Vars = unlist(Vars)

    Temp=aggregate(Vars, by = list(Weeks),FUN=meanNA)
    VarsSE = aggregate(Vars, by = list(Weeks),FUN=SE)[,2]
    VarsSD = aggregate(Vars, by = list(Weeks),FUN=sdNA)[,2]
    Temp2 = data.frame(Temp[,1],Temp[,2],VarsSD,VarsSE)
    colnames(Temp2) = c("WeekNum",paste0("Mean_",Var),paste0("SD_",Var),paste0("SE_",Var))
    Out2[[i]] = Temp2
    names(Out2) = paste0("Y",Years)
  }
  OutList = list(Out2,Weeks, Vars)
  names(OutList) = c("Means", "Weeks", "Vars")

  return(OutList)
}

#' Perform a correlation between the weekly average acros all sites of two ChemVars variables
#' @param Sites Which sites to include in the average, from names(ChemPrime) - character vector
#' @param Var1 First variable to include in the correlation, from ChemVars - single character name
#' @param Var2 Second variable to include in the correlation, from ChemVars - single character name
#' @param Years Which years to include in averaging Var1 and Var2 - character vector
#' @param method Method to use for the correlation, from ?cor.test - single character name
#' @keywords chemistry data
#' @export
#' @examples getChemAvg(Sites = names(ChemPrime),Var = ChemVars[11])
#' @returns Results from cor.test
corChemAvg = function(Var1,Var2, Years, Sites, method = "pearson"){
  Vars1 = getChemAvg(Sites = Sites, Var = Var1, Years = Years)
  Vars2 = getChemAvg(Sites = Sites, Var = Var2, Years = Years)

  Vars1Out = vector(mode = "list", length=length(Years))
  for (i in 1:length(Years)){
    Vars1Out[[i]] = Vars1$Means[[paste0("Y",Years[i])]][,2]
  }
  Vars1Out = unlist(Vars1Out)

  Vars2Out = vector(mode = "list", length=length(Years))
  for (i in 1:length(Years)){
    Vars2Out[[i]] = Vars2$Means[[paste0("Y",Years[i])]][,2]
  }
  Vars2Out = unlist(Vars2Out)

  OutList = list(cor.test(Vars1Out,Vars2Out),Vars1Out,Vars2Out)
  names(OutList) = c("cor.test",Var1,Var2)
  return(OutList)
}



