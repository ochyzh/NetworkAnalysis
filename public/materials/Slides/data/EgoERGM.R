#' Estimation of ego-Exponential Random Graph Model (ego-ERGM) using Expectation Maximization (EM) as per Salter-Townshend and Murphy (2015).
#'
#' This function estimates an ego-ERGM.  Code taken from Salter-Townshend and Murphy (2015)'s replication archive.
#' @param net The cross-sectional network that an ego-ERGM will be fit on.  Must be presented as a network object.  Any vertex attributes should be attached to networks.  Currently the function does not support comparisons of whole networks.
#' @param core_size The order of alters to include. The default value of one implies only looking at an ego's alters and the connections among them.
#' @param min_size  The minimum number of nodes an ego-network must achieve to be included.  Defaults to five.
#' @param roles The number of roles that should be fit.  Defaults to 3.
#' @param form The formula comprised of ERGM or TERGM terms used to distinguish between clusters assignments.  Specified as a vector of comma separated terms. No default.
#' @param directed Should the longitudinal network be treated as directed? If so, specify as the default TRUE.
#' @param edge_covariates Are edge covariates included in the form term? IF so, specify as TRUE.  No default.
#' @param seed The seed set to replicate analysis for pseudorandom number generator.
#' @param forking If parallelization via forking should be used (TRUE) or if no parallel processing should be used (FALSE).  Currently, sockets are not supported.
#' @param ncpus The number of CPUs that should should be used for estimation, defaults to 1.
#' @param steps The number of default EM steps that should be taken, defaults to 50.
#' @param tol The difference in parameter estimates between EM iterations to determine if the algorithm has converged.  Defaults to 1e-6.
#' @return A list of model results, including lambda (the probability of assignments), 
#' group.theta (the roles by terms cluster centroids),
#'         EE.BIC (the Salter-Townshend and Murphy BIC cross-sectional BIC),
#'        role_assignments (a data frame of the most likely assignments), and reduced_networks (network with excluded ego).
#' @keywords ego-ERGM
#' @references{
#'  Box-Steffensmeier, Janet M., Benjamin W. Campbell, Dino P. Christenson, Zachary Navabi. (2018):
#'  Role analysis using the ego-ERGM: A Look at environmental interest group coalitions.
#'  \emph{Social Networks} 52: 213-227.  \url{https://doi.org/10.1016/j.socnet.2017.08.004}
#'
#' Campbell, Benjamin W. (2018):
#'  Inferring Latent Roles in Longitudinal Networks.
#'  \emph{Political Analysis} 26(3): 292-311.  \url{https://doi.org/10.1017/pan.2018.20}
#'
#'  Salter-Townshend, Michael and Thomas Brendan Murphy. (2015):
#'  Role Analysis in Networks using Mixtures of Exponential Random Graph Models.
#'  \emph{Journal of Computational and Graphical Statistics} 24(2): 520-538. \url{https://doi.org/10.1080/10618600.2014.923777}
#' }
#' @examples
#' \donttest{
#' # Code from xergm.common and their preparation of the Knecht network
#' library(xergm.common)
#' set.seed(1)
#'
#' data("knecht")
#'
#' for (i in 1:length(friendship)) {
#'  rownames(friendship[[i]]) <- paste("Student.", 1:nrow(friendship[[i]]), sep="")
#'  colnames(friendship[[i]]) <- paste("Student.", 1:nrow(friendship[[i]]), sep="")
#' }
#' rownames(primary) <- rownames(friendship[[1]])
#' colnames(primary) <- colnames(friendship[[1]])
#' sex <- demographics$sex
#' names(sex) <- rownames(friendship[[1]])
#' # step 2: imputation of NAs and removal of absent nodes:
#' friendship <- xergm.common::handleMissings(friendship, na = 10, method = "remove")
#' friendship <- xergm.common::handleMissings(friendship, na = NA, method = "fillmode")
#' # step 3: add nodal covariates to the networks
#' for (i in 1:length(friendship)) {
#'   s <- xergm.common::adjust(sex, friendship[[i]])
#'   friendship[[i]] <- network::network(friendship[[i]])
#'   friendship[[i]] <- network::set.vertex.attribute(friendship[[i]], "sex", s)
#'   idegsqrt <- sqrt(sna::degree(friendship[[i]], cmode = "indegree"))
#'   friendship[[i]] <- network::set.vertex.attribute(friendship[[i]],
#'                                                    "idegsqrt", idegsqrt)
#'   odegsqrt <- sqrt(sna::degree(friendship[[i]], cmode = "outdegree"))
#'   friendship[[i]] <- network::set.vertex.attribute(friendship[[i]],
#'                                                    "odegsqrt", odegsqrt)
#' }
#' sapply(friendship, network::network.size)
#' net <- friendship
#' rm(list=setdiff(ls(), "net"))
#'
#' # Reduce down to first time-step
#' ego_ergm_fit <- ego_ergm(net = net[[1]],
#'                           form = c("edges", "mutual", "triangle",
#'                                    "nodeicov('idegsqrt')", "nodeocov('odegsqrt')",
#'                                    "nodematch('sex')"),
#'                           core_size = 1,
#'                           min_size = 5,
#'                           roles = 3,
#'                           forking = FALSE,
#'                           ncpus = 1,
#'                           directed = TRUE,
#'                           edge_covariates = FALSE,
#'                           seed = 12345,
#'                           steps = 50,
#'                           tol = 1e-06)
#' }
#' @export

ego_ergm <- function(net = NULL,
                     form = NULL,
                     core_size = 1, min_size = 5, roles = 3, directed = TRUE, edge_covariates = FALSE,
                     seed = 12345,
                     forking = FALSE, ncpus = 1,
                     steps = 50, tol = 1e-6){
  cat("Start Time:", format(Sys.time(), "%a %b %d %X %Y"), "\n")
  if (min_size<=1){
    stop("Minimum size must be greater than 1.  Please adjust the min_size argument to a value greater than 1.")
  }
  
  cat("Data formatting started.", "\n")
  # tested prior to 9/3/17
  set.seed(seed)
  N = network::network.size(net)
  orig_nets <- net
  
  ########################################################################
  ### Start Functions
  ###  (for network prep)
  ########################################################################
  
  vertices <- network::get.vertex.attribute(net, 'vertex.names')
  vertices <- vertices[order(vertices)]
  
  Y <- network::as.matrix.network(net)
  
  if(directed == FALSE){
    Y2 <- Y+t(Y)
    Y2[Y2 == 2] <- 1
    Y2 <- Y2[order(colnames(Y2)),order(colnames(Y2))]
  } else {
    Y2 <- Y
    Y2 <- Y2[order(colnames(Y2)),order(colnames(Y2))]
  }
  
  net2 <- network::network(Y2, directed = directed) #network object based upon the network matrix y which takes y and transforms it by	causing nodes to "jump backwards across links at the second step"
  
  x <- sna::gapply(net2,c(1,2),1:N,"*",1,distance=core_size)
  
  reduce_adjacency <- function(i){
    out <- as.matrix(Y[c(i,x[[i]]),c(i,x[[i]])])
    return(out)
  }
  
  if(forking == TRUE){
    x<-parallel::mclapply(seq_along(x), reduce_adjacency, mc.cores = ncpus, mc.preschedule = FALSE)
    # make all the adjacency matrices into network objects
    x<-parallel::mclapply(x, function(x) network::as.network.matrix(x, directed=directed), mc.cores = ncpus, mc.preschedule = FALSE)
  } else {
    x<-lapply(seq_along(x), reduce_adjacency)
    x<-lapply(x, function(x) network::as.network.matrix(x, directed=directed))
  }
  
  rm(Y2,net2)
  
  if(forking == TRUE){
    keep<-parallel::mclapply(x, network::network.size, mc.cores = ncpus, mc.preschedule = FALSE)>=min_size
  } else {
    keep<-lapply(x,network::network.size)>=min_size
  }
  
  all_net<-net
  N=all_net$gal$n
  #net<-network::network(network::as.sociomatrix(all_net)[(1:N)[keep],(1:N)[keep]], directed = FALSE)
  net<-network::delete.vertices(x = net, vid = which(get.vertex.attribute(net, 'vertex.names') %in% get.vertex.attribute(net, 'vertex.names')[keep==FALSE]))
  x<-x[keep]
  
  populate_attr <- function(i){
    tmp_net <- x[[i]]
    vertex_ids <- network::get.vertex.attribute(tmp_net, 'vertex.names')
    indices <- which(network::get.vertex.attribute(all_net, 'vertex.names') %in% vertex_ids)
    
    for(att in network::list.vertex.attributes(all_net)){
      network::set.vertex.attribute(tmp_net, att, network::get.vertex.attribute(all_net, att)[indices])
    }
    
    if(edge_covariates == TRUE){
      for(att_e in setdiff(network::list.network.attributes(all_net), c("bipartite", "directed", "hyper", "loops", "mnext", "multiple", "n"))){
        # el <- as.matrix(all_net,matrix.type="edgelist")
        #  el[,1] <- network::get.vertex.attribute(all_net, 'vertex.names')[el[,1]]
        #el[,2] <- network::get.vertex.attribute(all_net, 'vertex.names')[el[,2]]
        #el <- cbind(el, network::get.edge.attribute(all_net, att_e))
        
        adj <- as.matrix(all_net, matrix.type = "adjacency")
        adj <- adj[order(as.integer(colnames(adj))),order(as.integer(colnames(adj)))]
        
        #el_red <- as.matrix(x[[i]], matrix.type="edgelist")
        #el_red[,1] <- network::get.vertex.attribute(x[[i]], 'vertex.names')[el_red[,1]]
        #el_red[,2] <- network::get.vertex.attribute(x[[i]], 'vertex.names')[el_red[,2]]
        #el_red <- cbind(el_red, NA)
        adj_red <- as.matrix(tmp_net, matrix.type="adjacency")
        
        net_att <- network::get.network.attribute(all_net, att_e)
        
        colnames(net_att) <- network::get.vertex.attribute(all_net, 'vertex.names')
        rownames(net_att) <- network::get.vertex.attribute(all_net, 'vertex.names')
        
        vertices_red <- rownames(adj_red)
        net_red <- net_att[vertices_red, vertices_red]
        
        #if(all(el_red[,1] %in% el[,2])){
        #  el_red <- cbind(el_red[,2], el_red[,1], NA)
        #}
        
        #if(network::network.size(x[[i]]) == 1){
        #  el_red <- matrix(nrow = 0, ncol = 3)
        #}
        
        #m3 <- rbind(el, el_red)
        #el <- m3[duplicated(m3[,1:2], fromLast = TRUE), , drop = TRUE]
        
        #if(class(el) == "numeric" & length(el) == 3){
        #  el <- t(as.matrix(el))
        #}
        
        network::set.network.attribute(tmp_net,att_e,net_red)
      }
      
    }
    return(tmp_net)
  }
  
  if(forking == TRUE){
    x <- parallel::mclapply(seq_along(x), populate_attr, mc.cores = ncpus, mc.preschedule = FALSE)
  } else {
    x <- lapply(seq_along(x), populate_attr)
  }
  
  cat("Data formatting complete.", "\n")
  N=length(x)
  
  ### From likelihoods file
  ######## Specify the pseudologlikelihood ########
  LOWESTLL=-1e8
  pseudo.loglikelihood<-function(S,tmp.theta)  # Establish a pseudo-loglikelihood function
  {
    loglike=sum(dbinom(S$response*S$weights,S$weights,
                       boot::inv.logit(S$offset+as.matrix(S$predictor)%*%tmp.theta),log=TRUE),na.rm=1)
    if(!is.finite(loglike)|loglike<LOWESTLL)
      loglike=LOWESTLL# avoids numerical errors
    return(loglike)
  }
  # returns the negative so that optimization is towards maximum
  n.pseudo.loglikelihood<-function(S,tmp.theta){
    -pseudo.loglikelihood(S,tmp.theta)
  }
  
  approx.loglikelihood<-function(S,tmp.theta,old.theta,M,form,ll0){
    pseudo.loglikelihood(S,tmp.theta)
  }
  # loglikelihood summed across all groups
  Mstepfunction<-function(tmp.theta,S,N,lambda,TAU,G)
  {
    tmp.theta<-matrix(tmp.theta,nrow=G)
    ans=0
    for (g in 1:nrow(tmp.theta))
      ans = ans + mstepfunction(tmp.theta[g,],S,N,lambda[,g],TAU[g])
    return(ans)
  }
  n.approx.loglikelihood<-function(S,tmp.theta,old.theta,M,form,ll0){
    -approx.loglikelihood(S,tmp.theta,old.theta,M,form,ll0)
  }
  mstepfunction<-function(tmp.theta,S,N,lambda,TAU,old.theta,M,form,ll0){
    sum(lambda * (log(TAU) + sapply(S,approx.loglikelihood,tmp.theta,old.theta,M,form,ll0)))
  }
  n.mstepfunction<-function(tmp.theta,S,N,lambda,TAU,old.theta,M,form,ll0){
    -mstepfunction(tmp.theta,S,N,lambda,TAU,old.theta,M,form,ll0)
  }
  
  ego.terms <- form
  
  # fits the two-stage model which is also used to initialise the ego-ERGM EM algorithm.
  
  G <- roles
  
  # new
  init.egoergm<-function(ego.terms, G, p.ego.terms=NULL){ #Specify function in terms of ego.terms and G{
    Nterms<-length(ego.terms) #specifying a "nterms" object as the length of the number of ego.terms
    
    ######### fit all ego-networks #############
    if (is.null(p.ego.terms)){ #if p.ego terms is null or empty, then do the following...
      # new
      form = form
      fit_ergm_local <- function(i, form = NULL){
        form <- stats::formula(paste("x[[i]]~", paste(form,collapse="+"),sep=""))
        #Specify object ergmformula - paste command has three arguments - the stuff you want to paste together
        #, sep is how to separate them, and collapse is if you want smush it together.  This specifies ergmformula
        #as ~ pasted together with ego terms, separated by " "
        # form<-stats::update.formula(paste("x[[i]]",ergmformula),x[[i]] ~ .)
        fit = ergm::ergmMPLE(form,output="fit")$coef
        return(list(fit))
      }
      if(forking == TRUE){
        theta <- parallel::mclapply(seq_along(x), function(i) fit_ergm_local(i, form = form), mc.cores = ncpus, mc.preschedule = FALSE)
      } else {
        theta <- lapply(seq_along(x), function(i) fit_ergm_local(i, form = form))
      }
      
      theta<- do.call(rbind, unlist(theta, recursive = FALSE))
      theta <- apply(theta, 2, as.numeric)
      # next couple of lines very ad-hoc but not an issue post EM convergence.
      theta[is.na(theta)]<-0
      theta[theta==-Inf]<- -1e6
      theta[theta==Inf]<- 1e6
      ############# initialisation ##############
      # k-means Clustering start #if statement - if G>1, then the following is true and initial.clusters is set equal to
      #the kmeans of which takes the theta matrixm, for the number of centers, it is a function of theta, 2, and some other stuff?
      #I don't really understand this portion too much - go back to the article and see where this comes in?
      if (G>1)
      {
        initial.clusters<-stats::kmeans(theta, G, centers=apply(theta, 2, tapply,cutree(hclust(dist(theta)),G), mean),nstart=5)
        group.theta<-initial.clusters$centers
        group.theta<-matrix(group.theta,G,Nterms)
      }
      if (G==1){
        group.theta<-matrix(apply(theta,2,mean),nrow=1)
      }
      lambda<-matrix(NaN,N,G)
      for (i in 1:N){
        for (g in 1:G){
          lambda[i,g]<-1/(sqrt(t(group.theta[g,]-theta[i,])%*%(group.theta[g,]-theta[i,]))+tol) # nugget to avoid zero
        }
      }
      lambda<-lambda/apply(lambda,1,sum) # normalise lambda
      cat("Finished kmeans initialization.", "\n")
    }
    return(list(theta=theta, group.theta=group.theta, lambda=lambda, G=G))
  }
  
  set.seed(seed)
  init<-init.egoergm(ego.terms, G)
  
  ergmformula <- paste("~", paste(ego.terms,collapse="+"),sep="") # Establish function ergm formula that includes the ego.terms object
  obs.S<-list() #obs.S list that will be useful for the below for loop
  cat("Calculating all network statistics.","\n")
  
  calculate_change_statistics <- function(i){
    net<-x[[i]]
    cs <- ergm::ergmMPLE(stats::as.formula(paste("net",ergmformula)))
    cs$offset <- -log(network::network.size(net))
    return(cs)
  }
  
  ergmformula <- paste("~", paste(form,collapse="+"),sep="") # Establish function ergm formula that includes the ego.terms object
  
  if(forking == TRUE){
    obs.S <- parallel::mclapply(seq_along(x), calculate_change_statistics, mc.cores = ncpus, mc.preschedule = FALSE)
  } else {
    obs.S <- lapply(seq_along(x), calculate_change_statistics)
  }
  
  cat("Network statistics calculated.", "\n")
  
  
  fit.mix.egoergm<-function(ego.terms, init, obs.S, G, p.ego.terms=NULL, p.group.theta=NULL, N = length(obs.S), x = x, STEPS = 500, M)
    # Specify a function to perform EM algorithm to find ego-ERGM parameters and memberships.
  {
    Nterms<-length(ego.terms)
    ergmformula <- paste("~", paste(ego.terms,collapse="+"),sep="")
    form<-stats::update.formula(as.formula(paste("x[[i]]",ergmformula)),x[[i]] ~ .)
    lambda<-init$lambda
    group.theta<-init$group.theta
    TAU<-apply(lambda,2,sum)/N
    LL<-NaN
    cat("iterations\tlog-likelood\n")
    ############# E-M iterations ##############
    for (l in 1:STEPS)
    {
      oldLL<-LL
      #cat(l, " ", sep="")
      # E-step
      #cat("E-step", l, "\t")
      old_lambda<-lambda
      for (i in 1:N){
        for (g in 1:G){
          lambda[i,g]<-log(TAU[g]) + approx.loglikelihood(obs.S[[i]],group.theta[g,],group.theta[g,]*0,M,form,0)
        }
        lambda[i,]<-lambda[i,]-max(lambda[i,])
      }
      lambda<-exp(lambda)
      lambda<-lambda/apply(lambda,1,sum)
      lambda[is.na(lambda)]<-1/G
      tmp<-apply(lambda,1,sum)
      lambda<-lambda/tmp # normalise lambda
      lambda[lambda==0]<-1e-8; lambda<-lambda/tmp
      TAU<-apply(lambda,2,sum)/N
      # M-step
      #cat("M-step", l, "\t")
      LL<-Mstepfunction(group.theta,obs.S,N,lambda,TAU,G)
      cat(l, "\t", sprintf("%.10f",LL),"\n")
      old_group.theta<-group.theta
      for (g in 1:G)
        group.theta[g,]<-nlm(n.mstepfunction,group.theta[g,],S=obs.S,N=N,lambda=lambda[,g],TAU=TAU[g],group.theta[g,]*0,M,form,0)$estimate
      #if (max(abs(group.theta-old_group.theta))<tol)
      if (l>1)
        if ((LL-oldLL) < tol)
        {
          cat("Converged after ", l, "iterations\n")
          break
        }
    }
    #LL<-Mstepfunction(group.theta,obs.S,N,lambda,TAU,G)
    # higher is better
    EE.BIC<- 2*LL - (2*G*Nterms+G-1)*log(N) # usual BIC; higher is better
    return(list(EE.BIC=EE.BIC, theta=group.theta, lambda=lambda, G=G))
  }
  
  
  cat("EM algorithm starting.", "\n")
  out<-fit.mix.egoergm(ego.terms,init,obs.S,G)
  
  lambda<-out$lambda
  group.theta<-out$theta
  EE.BIC<-out$EE.BIC
  z<-apply(lambda, 1, which.max)
  roles_out <- data.frame(Id = network::get.vertex.attribute(net, 'vertex.names'),
                          Role = z)
  cat("EM algorithm completed.", "\n")
  cat("Done.", "\n")
  cat("Completed Time:", format(Sys.time(), "%a %b %d %X %Y"), "\n")
  return(list(model.fit = "egoERGM",
              lambda = lambda,
              group.theta = group.theta,
              EE.BIC = EE.BIC,
              role_assignments = roles_out,
              reduced_networks = net,
              form = form))
  
}