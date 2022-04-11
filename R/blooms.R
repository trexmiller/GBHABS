#' Detect blooms based on a variable (expects Phy_Spec) two standard deviations above the mean and at least 50 or higher
#' @param x A list of dataframes inherited from agrWeek(getChem())
#' @keywords chemistry data
#' @export
#' @import zoo
#' @examples bloomDetect(x)
#' @returns A list of 2- column dataframes for each year at one site containing sample dates and var concentrations
bloomDetect = function(x){
  BloomsOut = vector(mode = "list", length = length(Years))
  for (i in 1:length(Years)){
    Temp = x[[paste0("Y",Years[i])]]$Var
    Weeks = x[[paste0("Y",Years[i])]]$Week
    SD=sd(Temp,na.rm=TRUE)
    Mean = mean(Temp,na.rm=TRUE)
    k = Mean + (SD*2)
    Blooms = Weeks[Temp>k & Temp>50]
    BloomsOut[[i]] = Blooms[!is.na(Blooms)]
  }
  names(BloomsOut) = paste0("Y",Years)
  return(BloomsOut)
}

#Function to get conditions (chemistry variables) in bloom samples
getCond = function(Years,b,Site,Vars){
  Years2 = NULL
  for (i in 1:length(b)){
    if(length(b[[i]])>0){
      Years2[i] = Years[i]
    } else {
      Years2[i] = NA
    }
  }
  Years3 = Years2[!is.na(Years2)]

  BloomCond = vector(mode = "list", length = length(Years3))
  for (j in 1:length(Years3)){
    Year = Years3[j]
    blooms = b[[paste0("Y",Year)]]
    Weeks = agrWeek(getChem(s = Site, v = Vars[1]))[[paste0("Y",Year)]][,1]

    VarsOut = matrix(nrow = length(Weeks), ncol = length(Vars))
    for (y in 1:length(Vars)){
      VarsOut[,y]=agrWeek(getChem(s = Site, v = Vars[y]))[[paste0("Y",Year)]][,2]
    }
    colnames(VarsOut) = Vars
    VarsOutDF = data.frame(Weeks,VarsOut)

    MatOut = matrix(nrow = length(Vars),ncol=length(blooms))
    for (i in 1:length(blooms)){
      MatOut[,i] = as.numeric(VarsOutDF[VarsOutDF$Weeks == blooms[i],2:ncol(VarsOutDF)])
    }
    colnames(MatOut) = paste("Week",blooms)
    BloomCond[[j]] = MatOut
  }

  names(BloomCond) = paste0("Y",Years3)
  NCOL = sum(unlist(lapply(BloomCond,ncol)))
  Index = unlist(lapply(BloomCond,ncol))
  MatCond = matrix(ncol = NCOL,nrow = length(Vars))
  for(i in 1:length(BloomCond)){
    Temp = BloomCond[[i]]
    if(i==1){
      Temp2 = Temp[,1:Index[i]]
      MatCond[,1:Index[1]] = Temp2
    } else {
      Pos = sum(Index[1:(i-1)])+1
      To = Pos+Index[i]-1
      MatCond[,Pos:To] = Temp[,1:Index[i]]
    }
  }

  CondDF = as.data.frame(MatCond)
  colnames(CondDF) = c(seq(1,ncol(CondDF),1))
  for (i in 1:length(BloomCond)){
    Temp = BloomCond[[i]]
    if(i==1){
      colnames(CondDF)[1:Index[1]] = paste(names(BloomCond)[i],Site,colnames(Temp)[1:Index[1]])
    } else {
      Pos = sum(Index[1:(i-1)])+1
      To = Pos+Index[i]-1
      colnames(CondDF)[Pos:To] = paste(names(BloomCond)[i],Site,colnames(Temp)[1:Index[i]])
    }
  }
  return(CondDF)
}


PlotBlooms = function(b,p){
  quartz()
  par(mfrow = c(3,2))
  Years2 = NULL
  for (z in 1:length(Years)){
    if(length(b[[z]])>0){
      Years2[z] = Years[z]
    } else {
      Years2[z] = NA
    }
  }
  Years3 = Years2[!is.na(Years2)]

  for (i in 1:length(Years3)){
    BloomX = b[[paste0("Y",Years3[i])]]
    BloomY = p[[paste0("Y",Years3[i])]]

    BloomYPhyco = NULL
    for (j in 1:length(BloomX)){
      BloomYPhyco[j] = BloomY[BloomY[,1]==BloomX[j],2]
    }

    Weeks = p[[paste0("Y",Years3[i])]]$WeekNum
    Phyco = p[[paste0("Y",Years3[i])]]$Var

    plot(Weeks, Phyco, type = "o", pch = 21, bg = "blue", cex= 1.5)
    lines(BloomX,BloomYPhyco,col="red",cex = 2.5,type = "p")
  }
}
