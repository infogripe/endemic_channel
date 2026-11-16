data.prep <- function(cases,   
                      Pop = 1,
                      Date = NULL,
                      week = if(!is.null(Date)) lubridate::epiweek(Date),
                      Year = if(!is.null(Date)) lubridate::epiyear(Date)
){
  tibble(cases, Pop, week, Year) %>% 
    mutate(
      week = ifelse(week == 53, 52, week),
    )
  }


pthreshold.inla <- function(data,   # dados - Data containing columns (cases, Pop, week, Year)
                             Year2forecast = 2024,
                             quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                             output.only = F,
                             likelihood = "nbinomial",
                             timeRE = "rw2",
                             cyclic = T,
                             # Monte Carlo
                             MC = FALSE, MC.size = 2000,
                             WAIC = F){
  
  data.aux = data #%>% dplyr::ungroup() %>%
  #   dplyr::transmute(
  #     cases = cases,
  #     Pop = Pop,
  #     Date = Date,
  #     week = lubridate::epiweek(Date),
  #     week = ifelse(week == 53, 52, week),
  #     Year = lubridate::epiyear(Date)
  #   )
  
  
  data.inla <- data.aux %>%
    dplyr::filter(Year < Year2forecast)
  
  # # Adding forecasting component
  # # Is the year to forecast in the dataset?
  # if(any(data.aux$Year == Year2forecast)){
  #   pop.aux = data.aux[data.aux$Year == Year2forecast,]$Pop[1]
  # }else{
  #   if(is.null(Pop.forecast)){
  #     pop.aux = data.aux[data.aux$Year == Year2forecast-1,]$Pop[1]
  #   }else{
  #     pop.aux = Pop.forecast
  #   }
  # }
  
  data.inla <- data.inla %>%
    dplyr::add_row(week = 1:52,
                   Year = Year2forecast,
                   # Pop = pop.aux
    ) %>%
    dplyr::mutate(year = Year - min(Year) + 1)
  
  
  formula.q <- cases ~ 1 +
    f(week, model = timeRE, constr = T, cyclic = cyclic,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    ) +
    f(year, model = "iid", constr = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    )
  
  
  
  
  linear.term.year.cur <- which(data.inla$Year == Year2forecast)
  
  
  output.mean <- INLA::inla(formula = formula.q,
                            data = data.inla,
                            control.predictor = list(link = 1, compute = T,
                                                     quantiles = quantiles),
                            family = likelihood,
                            offset = log(Pop / 1e5),
                            # control.family = list(
                            #   control.link = list(
                            #     model = "quantile",
                            #     quantile = 0.5
                            #     )
                            #   ),
                            # control.fixed = control.fixed(prec.intercept = 1),
                            control.compute = list(config = MC, waic = WAIC)
  )
  
  if(MC){
    # Sample from the approximated posterior distribution
    param.samples <- INLA::inla.posterior.sample(output.mean, n = MC.size)
    
    # Transform from the inla format to a data frame
    samples.MC <- param.samples %>%
      purrr::map(.f = function(xxx, idx = linear.term.year.cur){
        stats::rnbinom(
          n = idx,
          mu = exp(xxx$latent[idx]),
          size = xxx$hyperpar[1]
        )} )
    
    names(samples.MC) <- 1:MC.size
    
    samples.MC <- samples.MC %>%
      dplyr::bind_rows(.id = "samples") %>%
      tibble::rowid_to_column(var = "week") %>%
      tidyr::gather(key = "samples", value = "values", -week)
    
    out = samples.MC
    
  }else{
    
    if(output.only){
      
      out = output.mean
      
    } else{
      
      out = tibble::tibble(week = 1:52, Year = Year2forecast) %>%
        dplyr::bind_cols( output.mean$summary.fitted.values[linear.term.year.cur,] )
    }
    
  }
  
  
  out
}
