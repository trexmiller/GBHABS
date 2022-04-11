#' Get cell counts for a given site
#' @param Mat Phyto data frame or a dataframe subsetted from Phyto
#' @param Site One of the sampling sites from unique(Phyto$Sample.Location)
#' @param Taxa Defaults to "DNR.Parameter.Description"
#' @param Dates Defaults to "Sample.Collected..Start..Date",
#' @param Units Defaults to "Units"
#' @param UnitType Default is "cells/ml" but could be either "cells/mL" or "NU/ml" where NU/ml is natural units such as filaments or colonies
#' @param Result Defaults to "Result.Amt"
#' @keywords phytoplankton
#' @export
#' @examples GetCounts(Mat = Phyto, Site = unique(Phyto$Sample.Location)[1])
#' @returns A data frame of cell densities by sample date and species
GetCounts=function(Mat,Site,Locs="Sample.Location",Taxa="DNR.Parameter.Description",Dates="Sample.Collected..Start..Date",Units = "Units",UnitType="cells/mL",Result ="Result.Amt" ){
  BB=Mat[Mat[,Locs]==Site,]
  Tax=unique(BB[,Taxa])
  BB.Dates=as.POSIXct(as.character(BB[,"Sample.Collected..Start..Date"]),format="%m/%d/%y")
  BB.Datesu=unique(BB.Dates)
  Abund=matrix(nrow=length(BB.Datesu),ncol=length(Tax))
  for(i in 1:length(Tax)){
    Temp=BB[BB[,"DNR.Parameter.Description"]==Tax[i],]
    Temp2=Temp[Temp[,Units]=="cells/mL",]
    Temp.Dates=as.POSIXct(Temp2[,"Sample.Collected..Start..Date"],format="%m/%d/%y",tz="America/Chicago")
    Out=NULL
    for(j in 1:length(BB.Datesu)){
      TempJ=Temp2[grep(BB.Datesu[j],Temp.Dates),Result]

      if (length(TempJ)==0){
        Out[j]=0
      } else {
        Out[j]=mean(TempJ)
      }

    }
    Abund[,i]=Out
  }

  Tax2=as.character(Tax)

  RemoveCount=function(x){
    unlist(strsplit(x, " "))[1]

  }
  Names=unlist(lapply(Tax2,RemoveCount))

  colnames(Abund)=Names
  row.names(Abund)=as.character(BB.Datesu)
  return(Abund)
}

#' Get the average cell density of a phylum across all sites
#' @param phylum A single phylum to average, given by unique(Phyto$PHYLUM.algaebase.org)
#' @param sites Which sites to use in the average given by unique(Phyto$Sample.Location). Multiple sites expected.
#' @keywords phytoplankton
#' @export
#' @examples taxaAvg(sites = unique(Phyto$Sample.Location),phylum = unique(Phyto$PHYLUM.algaebase.org)[1])
#' @returns A data frame of cell densities by sample date and species
taxaAvg = function(phylum,sites){
  Taxa=Phyto[Phyto[,"PHYLUM.algaebase.org"]==phylum,]
  TaxaCells=Taxa[Taxa[,"units"]=="cells/mL",]

  PhytoL=list(names=sites)
  for(i in 1:length(sites)){
    PhytoL[[i]]=GetCounts(Mat=TaxaCells,Site=sites[i])
  }
  names(PhytoL)=sites
  PhytoMean = lapply(PhytoL,colMeans)
  Species=vector(mode="list",length=length(PhytoMean))
  for (i in 1:length(PhytoMean)){
    Species[[i]] = names(PhytoMean[[i]])
  }
  Speciesu = unique(unlist(Species))

  CellMeans = NULL
  CellMeansOut = NULL
  for (i in 1:length(Speciesu)){
    for (j in 1:length(PhytoMean)){
      SpeciesMean = PhytoMean[[j]][names(PhytoMean[[j]])==Speciesu[i]]
      if (length(SpeciesMean)==0){
        CellMeans[j] = 0
      } else {
        CellMeans[j] = SpeciesMean
      }
    }
    CellMeansOut[i] = mean(CellMeans)
  }
  names(CellMeansOut) = Speciesu
  CellMeansOut2 = CellMeansOut[order(CellMeansOut)]
  return(CellMeansOut2)
}
