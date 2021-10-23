library(migrateR)
  #
  ## "MigrateR: 
  ## extending model-driven methods for quantifying migratory movement behavior"
  ##
  ## EXAMPLE ANALYSIS REPRODUCING MS RESULTS FROM "elk"
  ## D. Spitz
  #
  # I. Setup
  #  A. Load Package and Data
  #  B. Define Starting Parameter Values
  #
  # II. Fit and Refine Models
  #  A. NSD Models
  #  B. rNSD Models
  #  C. Elevation Models
  #
  # III. Identify Top Models	
  #  A. NSD
  #  B. rNSD
  #  C. Elevation
  #
  # IV Reproduce Plots (Figure 2)
  #
  # V. Reproduce Tables
  # TABLE 1. Comparisson of Classificaiton: NSD vs. rNSD
  # TABLE 2. Comparisson of Classificaiton: NSD vs. Elev	
  # TABLE 3. Between-method differences in parameter estimates 
  # 


  ## I. Setup
  ##----------------------------------------------------------------------------

  ## A. LOAD PACKAGE AND DATA		
  require(migrateR)	
  data(elk)
  
  ## B. DEFINE STARTING PARAMETER VALUES
  #this is the start date, to ensure that estimates of migratory timing are
  #comparable. This example has March 1 as the start date
    stdt <- "3-1"

  #this is defining the starting delta value, or distance separating seasonal ranges
  #This function can be used to manually specify starting values or restrict 
  #the range of terms included in movement models. Running pEst() returns 
  #a ‘data.frame’ containing default values. The missing values in this 
  #‘data.frame’ are filled dynamically by mvmtClass. By default, pEst specifies 
  #that migration can’t occur before the first location observed
    pest.n <- pEst()
    pest.n2 <- pEst(s.d = 90)
    pest.n3 <- pEst(s.d=100)
    pest.n4 <- pEst(s.d = 500)

	pest.e <- pEst(s.d = 1000)
	pest.e2 <- pEst(s.d = 2000)


  ## II. Fit and Refine Models
  ##----------------------------------------------------------------------------

    ##  A. NSD MODELS
    nsd <- mvmtClass(elk, p.est = pest.n, stdt = stdt)
	    #shows sum of bursts that are missing models, need to see if changing parameter 
	    #estimates will improve this
      sum(!fullmvmt(nsd)) # bursts missing 1 or more models
      
      #plots individual separately, press any key to go through all of them
      #(for some reason, cant use plot() bc you get an error)
      plot.mvmts(nsd)
      
      #plots first individual
      plot.mvmt(nsd[[1]])
    
      #Most problems with model fitting are a result of poor correspondence 
      #between bursts and parameter constraints or starting values. For 
      #datasets containing variable behavior, like elk and bighorn, a single 
      #set of parameter estimates may be insufficient for fitting all models 
      #and multiple sets may be required. A related but distinct problem is 
      #that poorly chosen starting values or constraints don’t always cause 
      #convergence failure, but can instead force mvmtClass to fit suboptimal 
      #models. The refine function provides an answers to both of these 
      #challenges. This function requires two arguments, output from 
      #mvmtClass and a new set of starting parameter values and constraints 
      #(supplied by pEst), attempts to refit each model using the new 
      #constraints, and compares these models to the original results, 
      #keeping only the single model of each type with the lowest AIC.
    nsd2 <- refine(nsd, pest.n2)
    
    #lists which bursts are missing models
      which(!fullmvmt(nsd2)) # bursts missing 1 or more models
    
    nsd3 <- refine(nsd2,pest.n3) 
      sum(!fullmvmt(nsd3)) # no bursts are missing models!

    plot.mvmts(nsd3)

##stopped here!!!
    
    
    ## B. rNSD MODELS	
    rlocs <- findrloc(elk,stdt=stdt,p.est=pest.n)  # calculate reference dates
    rlocs2 <- findrloc(elk,stdt=stdt,p.est=pest.n2)  # calculate reference dates
    rlocs4 <- findrloc(elk,stdt=stdt,p.est=pest.n4)  # calculate reference dates

cbind(rlocs$rloc,rlocs2$rloc,rlocs4$rloc)

    rnsd <- mvmtClass(elk, rloc = rlocs$rloc, p.est = pest.n, stdt = stdt)
      sum(!fullmvmt(rnsd)) # bursts missing 1 or more models

    rnsd2 <- refine(rnsd,pest.n2)
      sum(!fullmvmt(rnsd2)) # bursts missing 1 or more models

    rnsd3 <- refine(rnsd2,pest.n4)
      sum(!fullmvmt(rnsd3)) # bursts missing 1 or more models

    #  plot(rnsd3)


    ## C. ELEVATION MODELS
    elev <- mvmtClass(elk, fam = "elev", p.est = pest.e, stdt = stdt)
   	  sum(!fullmvmt(elev)) # bursts missing 1 or more models

    elev2 <- refine(elev, pest.e2)
   	  sum(!fullmvmt(elev2)) # bursts missing 1 or more models

    #  plot(elev2)


  ## III. Identify Top Models
  ##----------------------------------------------------------------------------
    nsd <- nsd3
    rnsd <- rnsd3
    elev  <- elev2

    ## A. NSD
	t.nsd <- topmvmt(nsd)
	nt.nsd1  <- names(t.nsd)	# Names of Top NSD models
	nt.nsd <- gsub("mixmig","migrant",nt.nsd1)
	table(nt.nsd)

    ## B. rNSD
    t.rnsd <- topmvmt(rnsd)
    nt.rnsd1 <- names(t.rnsd)		# Names of Top rNSD models
    nt.rnsd <- gsub("mixmig","migrant",nt.rnsd1) 	# collapse mig & mixmig
    table(nt.rnsd)

#      # rNSD for Comparison to Elevation -- nomads omitted
#      t.nsd2 <- topmvmt(nsd,omit="nomad")
#      nt.nsd2 <- gsub("mixmig","migrant",names(t.nsd2))
#      table(nt.nsd2)

			
    ## C. Elevation
    t.elev <- topmvmt(elev)
    nt.elev <- names(t.elev)	# Names of Top elevation models
    table(nt.elev)

  classifications <- data.frame(rnsd = nt.rnsd1, nsd = nt.nsd1, elev = nt.elev)	
  n <- length(nsd)


  ## TABLE 1. Comparisson of Classificaiton: NSD vs. rNSD
  ##----------------------------------------------------------------------------
  agr.rnsd <- nt.nsd==nt.rnsd		# agr(eement nsd &)rnsd: 
  table(nt.nsd[which(agr.rnsd)])	# consistent classifications
  prop.test(sum(!agr.rnsd),n)		# Estimate of Classificaiton Differences
  

    # Create transition matrix
    transMrnsd <- sapply(names(nsd[[1]]@models),function(x){
      sapply(names(nsd[[1]]@models),function(y){
        sum(classifications$nsd[classifications$rnsd==x]==y)
      })
    })
 	transMrnsd <- rbind(transMrnsd,total=apply(transMrnsd,2,sum))
   (transMrnsd <- cbind(transMrnsd,total=apply(transMrnsd,1,sum)))
    

  ## TABLE 2. Comparisson of Classificaiton: NSD vs. Elev
  ##----------------------------------------------------------------------------
  agr.elev <- nt.nsd==nt.elev		# agr(eement nsd &) elv
  table(nt.elev[which(agr.elev)])	# consistent classifications
  prop.test(sum(!agr.elev),n)		# Estimate of Classificaiton Differences

    # Create transition matrix
    transMelev <- sapply(names(nsd[[1]]@models),function(x){
      sapply(names(nsd[[1]]@models),function(y){
        sum(classifications$nsd[classifications$elev==x]==y)
      })
    })
 	transMelev <- rbind(transMelev,total=apply(transMelev,2,sum))
    (transMelev <- cbind(transMelev,total=apply(transMelev,1,sum)))


  ## TABLE 3. Between-method differences in parameter estimates 
  ##----------------------------------------------------------------------------
  # Create data.frames of parameter estimates from confirmed migrants

    ## NSD & rNSD : which trajectories agree and are migrant?
    mig.rnsd <- which(agr.rnsd)[which(grepl("migrant|mixmig",nt.nsd[agr.rnsd]))]
    
    # NSD- (confirmed) m(igrant)p(arameter estimates)			
    mp.nsd <- data.frame(t(sapply(t.nsd[mig.rnsd], function(x){
      coef(x)[1:5]
    })), row.names = burst(elk[mig.rnsd]))

	# rNSD- (confirmed) m(igrant)p(arameter estimates)
	mp.rnsd <- data.frame(t(sapply(t.rnsd[mig.rnsd], function(x){
      coef(x)[1:5]
    })), row.names = burst(elk[mig.rnsd]))

    ## Elev & NSD : which trajectories agree and are migrant?
    mig.elev <- which(agr.elev)[which(nt.elev[agr.elev]=="migrant")]

    # Elev- (confirmed) m(igrant)p(arameter estimates)
    mp.elev <- data.frame(t(sapply(t.elev[mig.elev], function(x){
      coef(x)[2:6]
    })), row.names = burst(elk[mig.elev]))

    # NSD2- (confirmed) m(igrant)p(arameter estimates)
    mp.nsd2 <- data.frame(t(sapply(t.nsd[mig.elev],function(x){
      coef(x)[1:5]
    })),row.names = burst(elk[mig.elev]))

			
  ## Create data.frame of rnsd results:
    # rNSD t-tests
    rnsd.tt <- sapply(names(mp.nsd),function(x){
        t.test(mp.nsd[,x],mp.rnsd[,x],paired=T)
      },simplify=F)
    rnsdt <- sapply(rnsd.tt,function(x){
        paste(round(x$estimate,2)," (p = ",round(x$p.value,2),
	    ", df = ",x$parameter, ")",sep="")
	  })

  migPrnsd <- data.frame(  
    Mean = round(apply(abs(mp.nsd-mp.rnsd),2,mean),2),
    SD = round(apply(abs(mp.nsd-mp.rnsd),2,sd),2),
    DD = rnsdt
  )[c("rho","theta","phi","phi2"),]


  ## Create data.frame of rnsd results:
	elev.tt <- sapply(names(mp.nsd),function(x){
	    t.test(mp.nsd2[,x],mp.elev[,x],paired=T)
	  },simplify=F)
	elevt <- sapply(elev.tt,function(x){
	    paste(round(x$estimate,2)," (p = ",round(x$p.value,4),
	    ", df = ",x$parameter, ")",sep="")
      })
  migPelev <- data.frame(  
    Mean = round(apply(abs(mp.nsd2-mp.elev),2,mean),2),
    SD = round(apply(abs(mp.nsd2-mp.elev),2,sd),2),
    DD = elevt
  )[c("rho","theta","phi","phi2"),]


  # Example Plots
  ##----------------------------------------------------------------------------
  plot(nsd[["YL15 2003"]])
  plot(rnsd[["YL15 2003"]])
  plot(elev[["YL15 2003"]])

  