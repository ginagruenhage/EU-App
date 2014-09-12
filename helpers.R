

compute.dmat <- function(df){
  vars.econ <- c('logPatents.pc','logIncome','logCO2.pc')
  vars.dem <- c('logFrate','logUPop.pc','logLE')
  E <- scale(df[,vars.econ])
  D <- scale(df[,vars.dem])
  D.econ <- a.m(dist(E))
  D.demo <- a.m(dist(D))
  Ds <- list(D.econ = D.econ, D.demo = D.demo)
  }

compute.weighted <- function(Ds, w, convex = NULL){
  if (convex) {
    sqrt(w*Ds$D.econ^2+(1-w)*Ds$D.demo^2)
    }
  else w*Ds$D.econ+(1-w)*Ds$D.demo
}
  
compute.Dlist <- function(Ds,T,convex){
  econ.weight <- seq(0,1,l=T)
  DistL <- llply(econ.weight,function(w) compute.weighted(Ds,w,convex))
  names(DistL) <- econ.weight
  DistL
}

make.df <- function(res,info.df){
  T <- length(res$DL)
  ids <- rownames(res$DL[[1]])
  if (length(names(res$DL))>0){
    alphaval <- as.numeric(names(res$DL))
  }
  else{
    alphaval <- 1:T
  }
  embed <- ldply(1:T, function(ind){
    df <- as.data.frame(t(res$XL[[ind]]))
    names(df) <- c("cmds.x1","cmds.x2")
    df[,"alpha"] <- alphaval[ind]
    df[,"id"] <- ids
    df
    })
  pen.df <- summary.cmds(res)$PenaltiesPerCurve
  names(pen.df) <- c("id","penalty")
  if (!missing(info.df)){
    embed <- merge(embed,info.df)
    embed <- merge(embed,pen.df)
  }
}

compute.limits <- function(embed){
  xlim <- c(min(embed$cmds.x1),max(embed$cmds.x1))
  ylim <- c(min(embed$cmds.x2),max(embed$cmds.x2))
  limits <- list(x = xlim, y = ylim)
  limits
  }

compute.alpha <- function(bins,a){
  alphavals <- seq(0,1,length = bins)
  alpha <- alphavals[a <= alphavals][1]
  alpha
  }

mycols = c(rgb(31,119, 180,max=255), rgb( 255,127,14,max=255), rgb( 44, 160, 44,max=255),rgb(214, 39,40,max=255),rgb(148,103,189,max=255),rgb( 140,86,75,max=255),rgb(227,119,194,max=255),rgb(127,127,127,max=255),rgb( 188,189,34,max=255),rgb(23,190,207,max=255)) 

theme_cmds_2d <- theme_classic() + theme(  axis.line = element_blank(),panel.border = element_rect(linetype = "solid",fill=NA, colour = "black"),legend.position="bottom",legend.key=element_rect(fill="white",colour="white"),legend.margin = unit(0,"cm"),plot.margin=unit(c(0,0,0,0),"cm"),strip.background=element_blank(),strip.text.x=element_blank()) 

theme_cmds_1d <- theme_classic() + theme( text = element_text(,size=10), axis.line = element_blank(), panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"), plot.margin=unit(c(0,0,0,0),"cm"))
                 



plot.timestep <- function(embed,a,limits,check.pen){

  all_values <- function(x){
  if (is.null(x)) return(NULL)
  
  row <- embed[embed$iso == x$iso,]
  row <- row[1, c("iso","country","IncomePerCapita","CO2.pc","Patents.pc","LifeExpectancyAtBirth","UrbanPop.pc","TotalFertilityRate")]
  paste0(names(row), ": ", format(row), collapse = "<br />")  
  }
  
  nf <- subset(embed, iso %in% c("NLD","FIN") & alpha < a + 1e-5 & alpha > a - 1e-5)
  be <- subset(embed, iso %in% c("BGR","EST") & alpha < a + 1e-5 & alpha > a - 1e-5)
  sub.df <- subset(embed, alpha < a + 1e-5 & alpha > a - 1e-5)
  if (check.pen){
    sub.df %>%
      ggvis(x = ~cmds.x1, y =  ~cmds.x2, fill = ~penalty, key := ~iso ) %>%
        layer_points(size := 50, stroke := "#081d58") %>%
          layer_text(dx := 5, dy := 0, text := ~iso, fill := "black", data = nf) %>%
              layer_text(dx := 5, text := ~iso, fill := "black", data = be) %>%
                scale_numeric("x", domain = limits$x, nice = TRUE) %>%
                  scale_numeric("y", domain = limits$y, nice = TRUE) %>%
                    scale_numeric("fill",range = c("#ffffd9","#081d58")) %>%
    add_legend("fill",title = "Penalty per Curve") %>%
    add_tooltip(all_values,"hover")
  } else {
    sub.df %>%
      ggvis(x = ~cmds.x1, y =  ~cmds.x2, key := ~iso ) %>%
        layer_points(size := 50, fill := mycols[1], fillOpacity := 0.8) %>%
          layer_points(size := 50, fill := mycols[4], data = nf ) %>%
            layer_text(dx := 5, dy := 0, text := ~iso, data = nf) %>%
              layer_text(dx := 5, text := ~iso, data = be) %>%
                layer_points(size := 50, fill := mycols[3], data = be ) %>%
        scale_numeric("x", domain = limits$x, nice = TRUE) %>%
          scale_numeric("y", domain = limits$y, nice = TRUE) %>%
            add_tooltip(all_values,"hover")
  }
}
