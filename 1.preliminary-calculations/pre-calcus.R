
# Scripts -----------------------------------------------------------------

source("internal-functions.R")
source("random-sampling.R")
source("elementary-effects.R")
source("methods.R")
source("auxiliar.R")

# Config ------------------------------------------------------------------

modelConfig  = file.path("config", "osmose_hum.csv")
modelConfig  = read.csv(file = modelConfig, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Process -----------------------------------------------------------------

# 1.   Predation accesibility                       : No parametrization ----
# 2-3. Predation size ratios (min, max) -------------------------------------

getAngles = function(sp, stages){
  
  max = as.numeric(as.vector(modelConfig[modelConfig[,1] == paste0("predation.predPrey.sizeRatio.max.", sp), (stages + 1)]))
  min = as.numeric(as.vector(modelConfig[modelConfig[,1] == paste0("predation.predPrey.sizeRatio.min.", sp), (stages + 1)]))
  
  theta.stage1 = angleEstimation(m_min = 0, m_max = 1/min[1])
  alpha.stage1 = angleEstimation(m_min = 0, m_max = 1/max[1]) - theta.stage1
  
  if(length(stages) == 1){
    return(c(theta.stage1, alpha.stage1)/ (pi/2))
  }
  
  if(length(stages) == 2){
    theta.stage2 = angleEstimation(m_min = 0, m_max = 1/min[2])
    alpha.stage2 = angleEstimation(m_min = 0, m_max = 1/max[2]) - theta.stage2
    
    return(c(theta.stage1, alpha.stage1, theta.stage2, alpha.stage2)/ (pi/2))
  }
  
  if(length(stages) == 3){
    
    theta.stage2 = angleEstimation(m_min = 0, m_max = 1/min[2])
    alpha.stage2 = angleEstimation(m_min = 0, m_max = 1/max[2]) - theta.stage2
    
    theta.stage3 = angleEstimation(m_min = 0, m_max = 1/min[3])
    alpha.stage3 = angleEstimation(m_min = 0, m_max = 1/max[3]) - theta.stage3
    
    return(c(theta.stage1, alpha.stage1, theta.stage2, alpha.stage2, theta.stage3, alpha.stage3)/ (pi/2))
  }
  
}

angles.sp0 = getAngles(sp = "sp0", stages = c(1,2))
angles.sp1 = getAngles(sp = "sp1", stages = c(1,2))
angles.sp2 = getAngles(sp = "sp2", stages = c(1,2))
angles.sp3 = getAngles(sp = "sp3", stages = c(1,2))
angles.sp4 = getAngles(sp = "sp4", stages = c(1,2))
angles.sp5 = getAngles(sp = "sp5", stages = c(1))
angles.sp6 = getAngles(sp = "sp6", stages = c(1))
angles.sp7 = getAngles(sp = "sp7", stages = c(1,2,3))
angles.sp8 = getAngles(sp = "sp8", stages = c(1,2))

x = matrix(c(angles.sp0, angles.sp1, angles.sp2,
             angles.sp3, angles.sp4, angles.sp5,
             angles.sp6, angles.sp7, angles.sp8))

rownames(x) = c(rep("sp0", length(angles.sp0)),
                rep("sp1", length(angles.sp1)),
                rep("sp2", length(angles.sp2)),
                rep("sp3", length(angles.sp3)),
                rep("sp4", length(angles.sp4)),
                rep("sp5", length(angles.sp5)),
                rep("sp6", length(angles.sp6)),
                rep("sp7", length(angles.sp7)),
                rep("sp8", length(angles.sp8)))
x[,1] = round(x[,1], 8)

write.csv(x, "uncertaintyAnalysis_preCalcus/angles.csv")


# 4.   Predation size threshold ---------------------------------------------

s0_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp0", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp0", 2])
s1_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp1", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp1", 2])
s2_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp2", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp2", 2])
s3_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp3", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp3", 2])
s4_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp4", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp4", 2])
s8_frac   = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp8", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp8", 2])

s7_s1 = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp7", 2])
s7_s2 = as.numeric(modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp7", 3])
Linf.sp7  = as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp7", 2])

s7_s1_frac = s7_s1 / Linf.sp7
s7_s2_frac = s7_s2 / Linf.sp7

s7_ratio = s7_s1 / s7_s2

threshold = c(s0_frac, s1_frac, s2_frac, s3_frac, s4_frac,
              s7_s2_frac, s7_ratio, s8_frac)


# 5.   Maximum starvation mortality rate            : No parametrization ----
# 6.   von Bertalanffy threshold --------------------------------------------

vbThrs_sp0 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp0", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp0", 2])
vbThrs_sp1 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp1", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp1", 2])
vbThrs_sp2 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp2", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp2", 2])
vbThrs_sp3 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp3", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp3", 2])
vbThrs_sp4 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp4", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp4", 2])
vbThrs_sp5 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp5", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp5", 2])
vbThrs_sp6 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp6", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp6", 2])
vbThrs_sp7 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp7", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp7", 2])
vbThrs_sp8 = as.numeric(modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp8", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp8", 2])

# 7.   Egg size                                     : No parametrization ----
# 8.   Critical thershold of predation efficiency   : No parametrization ----
# 9.   Maximum rate of ingestion of predator species: No parametrization ----
# 10.  Natural mortality rate                       : No parametrization ----
# 11.  Larval mortality rate ------------------------------------------------

larvaMortality.sp0 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-anchovy.csv"    , header = TRUE, sep = ";")
larvaMortality.sp1 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-hake.csv"       , header = TRUE, sep = ";")
larvaMortality.sp2 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-sardine.csv"    , header = TRUE, sep = ";")
larvaMortality.sp3 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-jurel.csv"      , header = TRUE, sep = ";")
larvaMortality.sp4 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-caballa.csv"    , header = TRUE, sep = ";")
larvaMortality.sp5 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-meso.csv"       , header = TRUE, sep = ";")
larvaMortality.sp6 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-munida.csv"     , header = TRUE, sep = ";")
larvaMortality.sp7 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-pota.csv"       , header = TRUE, sep = ";")
larvaMortality.sp8 = read.csv(file = "osmose-hum_v3/input/larval/larval_mortality-euphausidos.csv", header = TRUE, sep = ";")

larvaMortality.sp0 = larvaMortality.sp0$x
larvaMortality.sp1 = larvaMortality.sp1$x
larvaMortality.sp2 = larvaMortality.sp2$x
larvaMortality.sp3 = larvaMortality.sp3$x
larvaMortality.sp4 = larvaMortality.sp4$x
larvaMortality.sp5 = larvaMortality.sp5$x
larvaMortality.sp6 = larvaMortality.sp6$x
larvaMortality.sp7 = larvaMortality.sp7$x
larvaMortality.sp8 = larvaMortality.sp8$x

get_mlx = function(larvalVector){
  
  lx  = log(larvalVector)
  mlx = mean(lx)
  Lx = exp(mlx)
  
  return(Lx)
}

Lx_sp0 = get_mlx(larvaMortality.sp0)
Lx_sp1 = get_mlx(larvaMortality.sp1)
Lx_sp2 = get_mlx(larvaMortality.sp2)
Lx_sp3 = get_mlx(larvaMortality.sp3)
Lx_sp4 = get_mlx(larvaMortality.sp4)
Lx_sp5 = get_mlx(larvaMortality.sp5)
Lx_sp6 = get_mlx(larvaMortality.sp6)
Lx_sp7 = get_mlx(larvaMortality.sp7)
Lx_sp8 = get_mlx(larvaMortality.sp8)

# 12.  Fishing mortality multiplier                 : No parametrization ----
# 13.  Sex ratio                                    : No parametrization ----
# 14.  L0 -------------------------------------------------------------------

getL0 = function(sp){
  
  t0    = as.numeric(modelConfig[modelConfig[,1] == paste0("species.t0.", sp), 2]  )
  k     = as.numeric(modelConfig[modelConfig[,1] == paste0("species.K.", sp), 2])  
  Linf  = as.numeric(modelConfig[modelConfig[,1] == paste0("species.lInf.", sp), 2])  
  t = 0
  
  l0 = Linf*(1 - exp(-k * (t - t0)))
  
  return(l0)
}

l0_sp0 = getL0(sp = "sp0")
l0_sp1 = getL0(sp = "sp1")
l0_sp2 = getL0(sp = "sp2")
l0_sp3 = getL0(sp = "sp3")
l0_sp4 = getL0(sp = "sp4")
l0_sp5 = getL0(sp = "sp5")
l0_sp6 = getL0(sp = "sp6")
l0_sp7 = getL0(sp = "sp7")
l0_sp8 = getL0(sp = "sp8")

# 15.  k (von bertalanffy)                          : No parametrization ----
# 16.  Linf (von bertalanffy)                       : No parametrization ----
# 17.  Size at maturity -----------------------------------------------------

maturity_sp0 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp0", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp0", 2])
maturity_sp1 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp1", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp1", 2])
maturity_sp2 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp2", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp2", 2])
maturity_sp3 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp3", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp3", 2])
maturity_sp4 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp4", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp4", 2])
maturity_sp5 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp5", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp5", 2])
maturity_sp6 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp6", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp6", 2])
maturity_sp7 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp7", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp7", 2])
maturity_sp8 = as.numeric(modelConfig[modelConfig[,1] == "species.maturity.size.sp8", 2]) / as.numeric(modelConfig[modelConfig[,1] == "species.lInf.sp8", 2])



# 18.  Length-weigth relationship                   : No parametrization ----

