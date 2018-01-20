if (("msm" %in% rownames(installed.packages())) == FALSE){
  install.packages("msm")
}
if (("parallel" %in% rownames(installed.packages())) == FALSE){
  install.packages("parallel")
}
library(shiny)
library(msm)
library(parallel)


shinyServer(
  function(input, output, session){
    
    ### Define prior reactives based on user inputs
    prior_samps = reactive(
      {
        ## Prior for pop. total = negative binomial
        ## Prior for pop. proportion = beta
        if (input$tprior == 'nbinom' & input$pprior == 'beta'){
          n_picked <- 11 # The number of socks to pick out of the laundry
          
          sim_func = function(Mu, Sd, alpha, beta){
            prior_size <- -Mu^2 / (Mu - Sd^2)
            n_socks <- rnbinom(1, mu = Mu, size = prior_size)
            prop_pairs <- rbeta(1, shape1 = alpha, shape2 = beta)
            n_pairs <- round(floor(n_socks / 2) * prop_pairs)
            n_odd <- n_socks - n_pairs * 2
            
            # Simulating picking out n_picked socks
            socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks <- sample(socks, size =  min(n_picked, n_socks))
            sock_counts <- table(picked_socks)
            
            # Returning the parameters and counts of the number of matched 
            # and unique socks among those that were picked out.
            x = c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
                  n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
            
            return(x)
          }
          
          # Setting variables to be used in sampling
          nbm = input$nbprior_mu
          ns = input$n_sims
          nbsd = input$nbprior_sd
          a = input$shape1
          b = input$shape2
          
          # Generative model
          sock_sim = cbind(mclapply(rep(nbm, ns), sim_func, Sd = nbsd, alpha = a, beta = b, mc.cores = detectCores()))
          
          # Combine samples into a single matrix
          sock_sim <- do.call(rbind, sock_sim)
          
          return(sock_sim)
        }
        
        ## Prior for pop. total = negative binomial
        ## Prior for pop. proportion = truncated normal
        if (input$tprior == 'nbinom' & input$pprior == 'truncnorm'){
          n_picked <- 11 # The number of socks to pick out of the laundry
          
          sim_func = function(Mu1, Sd1, Mu2, Sd2){
            prior_size <- -Mu1^2 / (Mu1 - Sd1^2)
            n_socks <- rnbinom(1, mu = Mu1, size = prior_size)
            prop_pairs <- rtnorm(1, lower = 0, upper=1, mean = Mu2 , sd = Sd2)
            n_pairs <- round(floor(n_socks / 2) * prop_pairs)
            n_odd <- n_socks - n_pairs * 2
            
            # Simulating picking out n_picked socks
            socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks <- sample(socks, size =  min(n_picked, n_socks))
            sock_counts <- table(picked_socks)
            
            # Returning the parameters and counts of the number of matched 
            # and unique socks among those that were picked out.
            x = c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
                  n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
            
            return(x)
          }
          
          # Setting variables to be used in sampling
          nbm = input$nbprior_mu
          ns = input$n_sims
          nbsd = input$nbprior_sd
          tnm = input$tnprior_mu
          tnsd = input$tnprior_sd
          
          # Generative model
          sock_sim = cbind(mclapply(rep(nbm, ns), sim_func, Sd1 = nbsd, Mu2 = tnm, Sd2 = tnsd, mc.cores = detectCores()))
          
          # Combine samples into a single matrix
          sock_sim <- do.call(rbind, sock_sim)
          
          return(sock_sim)
        }  
        
        ## Prior for pop. total = normal
        ## Prior for pop. proportion = beta
        if (input$tprior == 'norm' & input$pprior == 'beta'){
          n_picked <- 11 # The number of socks to pick out of the laundry
          
          sim_func = function(Mu, Sd, alpha, beta){
            n_socks <- floor(rtnorm(1, lower = 0, mean = Mu, sd = Sd))
            prop_pairs <- rbeta(1, shape1 = alpha, shape2 = beta)
            n_pairs <- round(floor(n_socks / 2) * prop_pairs)
            n_odd <- n_socks - n_pairs * 2
            
            # Simulating picking out n_picked socks
            socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks <- sample(socks, size =  min(n_picked, n_socks))
            sock_counts <- table(picked_socks)
            
            # Returning the parameters and counts of the number of matched 
            # and unique socks among those that were picked out.
            x = c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
                  n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
            
            return(x)
          }
          
          # Setting variables to be used in sampling
          nm = input$nprior_mu
          ns = input$n_sims
          nsd = input$nprior_sd
          a = input$shape1
          b = input$shape2
          
          # Generative model
          sock_sim = cbind(mclapply(rep(nm, ns), sim_func, Sd = nsd, alpha = a, beta = b, mc.cores = detectCores()))
          
          # Combine samples into a single matrix
          sock_sim <- do.call(rbind, sock_sim)
          
          return(sock_sim)
        }
        
        ## Prior for pop. total = normal
        ## Prior for pop. proportion = truncated normal
        if (input$tprior == 'norm' & input$pprior == 'truncnorm'){
          n_picked <- 11 # The number of socks to pick out of the laundry
          
          sim_func = function(Mu1, Sd1, Mu2, Sd2){
            n_socks <- floor(rtnorm(1, lower = 0, mean = Mu1, sd = Sd1))
            prop_pairs <- rtnorm(1, lower = 0, upper=1, mean = Mu2 , sd = Sd2)
            n_pairs <- round(floor(n_socks / 2) * prop_pairs)
            n_odd <- n_socks - n_pairs * 2
            
            # Simulating picking out n_picked socks
            socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks <- sample(socks, size =  min(n_picked, n_socks))
            sock_counts <- table(picked_socks)
            
            # Returning the parameters and counts of the number of matched 
            # and unique socks among those that were picked out.
            x = c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
                  n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
            
            return(x)
          }
          
          # Setting variables to be used in sampling
          nm = input$nprior_mu
          ns = input$n_sims
          nsd = input$nprior_sd
          tnm = input$tnprior_mu
          tnsd = input$tnprior_sd
          
          # Generative model
          sock_sim = cbind(mclapply(rep(nm, ns), sim_func, Sd1 = nsd, Mu2 = tnm, Sd2 = tnsd, mc.cores = detectCores()))
          
          # Combine samples into a single matrix
          sock_sim <- do.call(rbind, sock_sim)
          
          return(sock_sim)
        }
      }
    )
    
    ### Define posterior reactives
    posterior_total = reactive(
      {
        #subset the generated samples to get those that match the observed total data
        prior_samps()[prior_samps()[, "unique"] == 11 & prior_samps()[, "pairs" ] == 0 , ][,3]
      }
    )
    posterior_prop = reactive(
      {
        #subset the generated samples to get those that match the observed proportion data
        prior_samps()[prior_samps()[, "unique"] == 11 & prior_samps()[, "pairs" ] == 0 , ][,6]
      }
    )
    
    ## Create posterior plot for total socks
    output$posterior_total_plot = renderPlot(
      {
        # plot posterior number of socks
        plot(density(posterior_total()), main = "Posterior for Total Number of Socks")
        
        # plot negative binomial prior
        if(input$tprior == 'nbinom' & input$totalprior == TRUE){
          lines(dnbinom(seq(0,120,by=1), size = -input$nbprior_mu^2 / (input$nbprior_mu - input$nbprior_sd^2), 
                        mu = input$nbprior_mu), lty = 2)
        }
        
        # plot normal prior
        if(input$tprior == 'norm' & input$totalprior == TRUE){
          lines(dtnorm(seq(0,120,by=1), lower = 0, mean = input$nprior_mu, 
                       sd = input$nprior_sd), lty= 2)
        }
        
        # create legend
        legend("topright", c("Mean","Median","Actual", "Prior"), lty=c(1,1,1,2), lwd=c(2,2,2,2), col=c('red','blue','darkgreen','black'))
        
        # draw mean, median, and true value
        if(input$totalest == TRUE){
          abline(v=mean(posterior_total()),col='red')
          abline(v=median(posterior_total()),col='blue')
        }
        if(input$totalval == TRUE){
          abline(v=45, col = 'darkgreen')
        }
      }
    )
    
    ## Create posterior plot for proportion of socks
    output$posterior_prop_plot = renderPlot(
      {
        # plot posterior
        plot(density(posterior_prop()), main = "Posterior for Proportion of Paired Socks")
        
        # plot beta prior
        if(input$pprior == 'beta' & input$propprior == TRUE){
          lines(seq(0,1,by=.01),dbeta(seq(0,1,by=.01), shape1 = input$shape1, shape2=input$shape2), lty= 2)
        }
        
        # plot truncated normal prior
        if(input$pprior == 'truncnorm' & input$propprior){
          lines(density(rtnorm(1000, lower = 0, upper = 1,
                               mean = input$tnprior_mu, sd = input$tnprior_sd)), lty = 2)
        }
        
        # create legend
        legend("topright", c("Mean","Median","Actual", "Prior"), lty=c(1,1,1,2), lwd=c(2,2,2,2), col=c('red','blue','darkgreen','black'))
        
        # draw mean, median, true value
        if(input$propest == TRUE){
          abline(v=mean(posterior_prop()),col='red')
          abline(v=median(posterior_prop()),col='blue')
        }
        if(input$propval == TRUE){
          abline(v=.93, col = 'darkgreen')
        }
      }
    )
    
    # Write text outputs
    output$TotalMean = renderPrint({
      cat("Total mean =", mean(posterior_total()))
    })
    
    output$TotalMedian = renderPrint({
      cat("Total median =", median(posterior_total()))
    })
    
    output$TotalCredibleInterval = renderPrint({
      cat("Total credible interval : ", quantile(posterior_total(), c(.05,.95)))
    })
    
    output$ProportionMean = renderPrint({
      cat("Proportion mean =", mean(posterior_prop()))
    })
    
    output$ProportionMedian = renderPrint({
      cat("Proportion median =", median(posterior_prop()))
    })
    output$ProportionCredibleInterval = renderPrint({
      cat("Proportion credible interval : ", quantile(posterior_prop(), c(.05,.95)))
    })
  }
)