# Extract City Boundary 
isInCity2_mat <- matrix(isInCity2,nrow=r@ncols,ncol=r@nrows)
Bndy_mat <- DetectEdge(isInCity2_mat)
isOnBndy <- as.vector(Bndy_mat) 
BndyNode <- round(bndyPercent*ENode)

NumDays.eval <- length(dateSeq.eval)

# A matrix storing the predicted crime rate maps where the row corresponds to (vectorized) regular grid (inside city area)
# and each column corresponds to date for evaluation
KDE.Hist_full <- KDEgrd.full[,c("X_COORD","Y_COORD")]
KDE.Hist_inCity <- KDE.Hist_full[isInCity2,]
RateMap_mesh.eval <- matrix(0,ncol=NumDays.eval,nrow=nrow(KDE.Hist_inCity)) 
# RateMap_pix.eval <- RateMap_mesh.eval

# Long-term historical inicident data are used for arranging mesh nodes 
# (One placement of mesh nodes will be fixed for all the evaluation days while the estimates of nodal values vary)
startDate.mesh <- startDate.eval-meshHistSpan
endDate.mesh <- startDate.eval-1
CrimeHistPts <- subset(CrimeData,DATEOCC>=startDate.mesh & DATEOCC<=endDate.mesh,select=c("X_COORD","Y_COORD","INC_CNT"))
CrimeHistPts.raster <- rasterize(CrimeHistPts[,c("X_COORD","Y_COORD")], r, CrimeHistPts$INC_CNT, fun=sum)
CrimeHistPts.df_inCity <- as.data.frame(CrimeHistPts.raster,xy=TRUE)[isInCity,]
names(CrimeHistPts.df_inCity) <- c("X_COORD","Y_COORD","INC_CNT")
CrimeHistPts.df_inCity$INC_CNT[is.na(CrimeHistPts.df_inCity$INC_CNT)] <- 0
CrimeHistPts.df_inCity <- CrimeHistPts.df_inCity[ord.R2K,] # convert pixel order from "RegGrd" to "KDEgrd"

# generate mesh grid
meshList <- generateConstraintMesh2(CrimeHistPts,r,KDEgrd.full,ENode,isOnBndy,BndyNode,bw=h,plot=FALSE)
MeshNode_raw <- meshList$meshNode
MeshTri <- meshList$Tri

# # generate pixel-based grid
# PixList <- generatePixGrd(ENode,RegGrd,r)
# PixGrd <- PixList$grd
# PixRaster <- PixList$raster

ptm <- proc.time()
for (i in 1:NumDays.eval){
  
  d <- dateSeq.eval[i]
  startDate.obs <- d-meshObsSpan
  endDate.obs <- d-1
  
  # Observed incidents are used for estimating nodal/pixel values
  CrimeObsPts <- subset(CrimeData,DATEOCC>=startDate.obs & DATEOCC<=endDate.obs,select=c("X_COORD","Y_COORD","INC_CNT"))
  CrimeObsPts.raster <- rasterize(CrimeObsPts[,c("X_COORD","Y_COORD")], r, CrimeObsPts$INC_CNT, fun=sum)
  CrimeObsPts.df_full <- as.data.frame(CrimeObsPts.raster,xy=TRUE)
  names(CrimeObsPts.df_full) <- c("X_COORD","Y_COORD","INC_CNT")
  CrimeObsPts.df_full$INC_CNT[is.na(CrimeObsPts.df_full$INC_CNT)] <- 0
  
  # Used for mesh reconstruction (only needs grids inside the city area)
  CrimeObsPts.df_inCity <- CrimeObsPts.df_full[isInCity,]
  CrimeObsPts.df_inCity <- CrimeObsPts.df_inCity[ord.R2K,] # convert pixel order from "RegGrd" to "KDEgrd"
  
  # # Used for pixel reconstruction (needs grids of rectangular area)
  # CrimeObsPts.df_full <- CrimeObsPts.df_full[ord.R2K_full,] # convert pixel order from "RegGrd" to "KDEgrd"
  
  # Mesh-MAP recon
  Recon_city.meshMAP <- MeshRecon(CrimeObsPts.df_inCity,MeshNode_raw,MeshTri,KDEgrd,iterMax=iterMax,Estimation="MAP",SmoothParam=SmParam)$Recon.city
  Recon_city.meshMAP$DENVAL <- Recon_city.meshMAP$VALUE/sum(Recon_city.meshMAP$VALUE) 
  
  # # Pix-MAP recon
  # Recon_city.pixMAP <- PixRecon(CrimeObsPts.df_full,PixGrd,KDEgrd.full,iterMax=iterMax,Estimation="MAP",SmoothParam=SmParam,
  #                        Raster=PixRaster,type="rook",interpMethod='linear',reltol=reltol,eps=eps)
  # Recon_city.pixMAP <- Recon_city.pixMAP[isInCity2,]
  # Recon_city.pixMAP$DENVAL <- Recon_city.pixMAP$VALUE/sum(Recon_city.pixMAP$VALUE)
  
  RateMap_mesh.eval[,i] <- CrimeData.eval$TSpred[i] * Recon_city.meshMAP$DENVAL
  # RateMap_pix.eval[,i] <- CrimeData.eval$TSpred[i] * Recon_city.pixMAP$DENVAL
  
}
proc.time()-ptm

# convert pixel order from "KDEgrd" to "RegGrd"
RateMap_mesh.eval <- RateMap_mesh.eval[ord.K2R,]
# RateMap_pix.eval <- RateMap_pix.eval[ord.K2R,]
