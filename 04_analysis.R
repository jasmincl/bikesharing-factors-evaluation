library(glmnet)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(leaflet)
library(sf)
library(Hmisc)
library(lattice)
library(MASS)
library(pscl)
library(lmtest)

# read in data
trip_scaled <- readRDS("data/trip_scaled.rds")

# check missings
# only keep complete cases (most NAs come from missing demography data for 1 station)
colSums(is.na(trip_scaled))
trip_nona <-  trip_scaled[complete.cases(trip_scaled), ]

trip_model <- trip_nona

saveRDS(trip_model, "data/trip_model.rds")

# DATA DIAGNOSTICS  --------------------------------------------------------------------------------------------------------------
trip_model <- readRDS("data/trip_model.rds")

## correlation matrix   --------------------------------------------------------------
# Plot a correlation graph
## https://rstudio-pubs-static.s3.amazonaws.com/387791_e70fec227cf24709a865cc0407b1776b.html

# select model vars only
trip_modelvars <- trip_model %>%
  dplyr::select(     from_n_close_transport_scaled
                     , from_n_close_education_scaled
                     , from_n_close_health_scaled   
                     , from_n_close_leisure_scaled     
                     , to_n_close_leisure_scaled    
                     , to_n_close_transport_scaled  
                     , to_n_close_education_scaled  
                     , to_n_close_health_scaled     
                     , from_closest_station_dist_scaled 
                     , to_closest_station_dist_scaled        
                     , from_dist_city_center_scaled 
                     , to_dist_city_center_scaled            
                     , from_population_scaled 
                     , to_population_scaled                  
                     , route_time_scaled
                     , reachable_30                   
                     , loop_trip 
                     , from_perc_residential_scaled
                     , to_perc_residential_scaled
                     , from_perc_commercial_scaled
                     , to_perc_commercial_scaled
                     , from_perc_retail_scaled
                     , to_perc_retail_scaled
                     , from_perc_industrial_scaled
                     , to_perc_industrial_scaled 
                     , from_perc_citizen_notDE_scaled
                     , to_perc_citizen_notDE_scaled
                     , perc_cycleway_scaled)


# plot in data frame because too many variables
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = round((cormat)[ut],2),
    p = pmat[ut]
  )
}

# calcualte matrix
res <- rcorr(as.matrix(trip_modelvars))
corrm <- flattenCorrMatrix(res$r, res$P)
corrm$abs <- abs(corrm$cor)

## multicollinearity  --------------------------------------------------------------
# VIF test
trip_modelvars_vif <- trip_model %>%
  dplyr::select(loop_trip  
                ,from_closest_station_dist, to_closest_station_dist
                ,from_population, to_population
                , from_perc_park_dummy , from_perc_residential_dummy
                ,from_perc_commercial_dummy, from_perc_retail_dummy, 
                from_perc_industrial_dummy
                , to_perc_park_dummy , to_perc_residential_dummy
                ,to_perc_commercial_dummy, to_perc_retail_dummy, 
                to_perc_industrial_dummy
                ,route_time
                ,citizen_notDE
                ,reachable_30
  )

library(usdm)
vif(as.data.frame(trip_modelvars))

# test for zero-inflation ----------------------------------------------------------------------------------------
zeros_absolute <- sum(trip_model$trip_count == 0)
zeros_perc <- 100*zeros_absolute/nrow(trip_model) # 87 % zeros

# expected number of zeros, actual number --------------------------------------------------------------
lambda_est <- mean(trip_model$trip_count)

p0_tilde <- exp(-lambda_est)
p0_tilde
n0 <- sum(1*(!(trip_model$trip_count >0)))
n <- length(trip_model$trip_count)

# number of observtions 'expected' to be zero
zeros_absolute-(n*p0_tilde)

#now lets perform the JVDB score test
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)

test_stat <- numerator/denominator

pvalue <- pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
pvalue  # --> it is significant

# overdispersion ------------------------------------

# MODELS --------------------------------------------------------------------------------------------------------------


# poisson --------------------------------------------------------------------------------------------------------------
m_poi <- glm(trip_count ~  
               from_n_close_transport_scaled
             + from_n_close_shopping_scaled 
             + from_n_close_education_scaled
             + from_n_close_health_scaled   
             + from_n_close_leisure_scaled     
             + to_n_close_leisure_scaled    
             + to_n_close_transport_scaled  
             + to_n_close_shopping_scaled   
             + to_n_close_education_scaled  
             + to_n_close_health_scaled     
             + from_closest_station_dist_scaled 
             + to_closest_station_dist_scaled        
             + from_dist_city_center_scaled 
             + to_dist_city_center_scaled            
             + from_population_scaled 
             + to_population_scaled                  
             + route_time_scaled
             + reachable_30                   
             + loop_trip 
             + weekday_cat
             + from_perc_residential_scaled
             + to_perc_residential_scaled
             + from_perc_commercial_scaled
             + to_perc_commercial_scaled
             + from_perc_retail_scaled
             + to_perc_retail_scaled
             + from_perc_industrial_scaled
             + to_perc_industrial_scaled,
             family = 'poisson',
             data = trip_model)


# zero inflated poisson --------------------------------------------------------------------------------------------------------------
m_poi0 <- zeroinfl(trip_count ~  
                     from_n_close_transport_scaled
                   + from_n_close_shopping_scaled 
                   + from_n_close_education_scaled
                   + from_n_close_health_scaled   
                   + from_n_close_leisure_scaled     
                   + to_n_close_leisure_scaled    
                   + to_n_close_transport_scaled  
                   + to_n_close_shopping_scaled   
                   + to_n_close_education_scaled  
                   + to_n_close_health_scaled     
                   + from_closest_station_dist_scaled 
                   + to_closest_station_dist_scaled        
                   + from_dist_city_center_scaled 
                   + to_dist_city_center_scaled            
                   + from_population_scaled 
                   + to_population_scaled                  
                   + route_time_scaled
                   + reachable_30                   
                   + loop_trip 
                   + weekday_cat
                   + from_perc_residential_scaled
                   + to_perc_residential_scaled
                   + from_perc_commercial_scaled
                   + to_perc_commercial_scaled
                   + from_perc_retail_scaled
                   + to_perc_retail_scaled
                   + from_perc_industrial_scaled
                   + to_perc_industrial_scaled | route_time,
                   data = trip_model,
                   dist = "poisson")


# NEGATIVE BINOMIAL  --------------------------------------------------------------------------------------------------------------

#   negative binomial base model --------------------------------------------------------------------------------------------------------------
m_negbi_base <- glm.nb(trip_count ~             
                         + from_population_scaled + to_population_scaled                  
                       + route_time_scaled,
                       data = trip_model)
#  zero-inflated negative binomial base model  --------------------------------------------------------------------------------------------------------------
m_negbi0_base <- zeroinfl(trip_count ~             
                            + from_population_scaled + to_population_scaled                  
                          + route_time_scaled | route_time,
                          data = trip_model,
                          dist = "negbin")



#   zero-inflated negative binomial pois close added --------------------------------------------------------------------------------------------------------------
m_negbi0_pois <- zeroinfl(trip_count ~  
                            from_n_close_transport_scaled
                          + from_n_close_education_scaled
                          + from_n_close_health_scaled   
                          + from_n_close_leisure_scaled     
                          + to_n_close_leisure_scaled    
                          + to_n_close_transport_scaled  
                          + to_n_close_education_scaled  
                          + to_n_close_health_scaled     
                          + from_closest_station_dist_scaled 
                          + to_closest_station_dist_scaled        
                          + from_dist_city_center_scaled 
                          + to_dist_city_center_scaled            
                          + from_population_scaled 
                          + to_population_scaled                  
                          + route_time_scaled
                          + reachable_30                   
                          + loop_trip 
                          + weekday_cat |route_time,
                          data = trip_model,
                          dist = "negbin")


#   zero-inflated negative binomial landuse added --------------------------------------------------------------------------------------------------------------
m_negbi0_landuse <- zeroinfl(trip_count ~  
                               from_n_close_transport_scaled
                             + from_n_close_education_scaled
                             + from_n_close_health_scaled   
                             + from_n_close_leisure_scaled     
                             + to_n_close_leisure_scaled    
                             + to_n_close_transport_scaled  
                             + to_n_close_education_scaled  
                             + to_n_close_health_scaled     
                             + from_closest_station_dist_scaled 
                             + to_closest_station_dist_scaled        
                             + from_dist_city_center_scaled 
                             + to_dist_city_center_scaled            
                             + from_population_scaled 
                             + to_population_scaled                  
                             + route_time_scaled
                             + reachable_30                   
                             + loop_trip 
                             + weekday_cat
                             + from_perc_residential_scaled
                             + to_perc_residential_scaled
                             + from_perc_commercial_scaled
                             + to_perc_commercial_scaled
                             + from_perc_retail_scaled
                             + to_perc_retail_scaled
                             + from_perc_industrial_scaled
                             + to_perc_industrial_scaled | route_time,
                             data = trip_model,
                             dist = "negbin")

#   negative binomial cycle path added --------------------------------------------------------------------------------------------------------------
m_negbi_all <- glm.nb(trip_count ~  
                        from_n_close_transport_scaled
                      + from_n_close_education_scaled
                      + from_n_close_health_scaled   
                      + from_n_close_leisure_scaled     
                      + to_n_close_leisure_scaled    
                      + to_n_close_transport_scaled  
                      + to_n_close_education_scaled  
                      + to_n_close_health_scaled     
                      + from_closest_station_dist_scaled 
                      + to_closest_station_dist_scaled        
                      + from_dist_city_center_scaled 
                      + to_dist_city_center_scaled            
                      + from_population_scaled 
                      + to_population_scaled                  
                      + route_time_scaled
                      + reachable_30                   
                      + loop_trip 
                      + weekday_cat                      
                      + from_perc_residential_scaled
                      + to_perc_residential_scaled
                      + from_perc_commercial_scaled
                      + to_perc_commercial_scaled
                      + from_perc_retail_scaled
                      + to_perc_retail_scaled
                      + from_perc_industrial_scaled
                      + to_perc_industrial_scaled 
                      + perc_cycleway_scaled,
                      data = trip_model)

#   zero-inflated negative binomial cycle path added --------------------------------------------------------------------------------------------------------------
m_negbi0_all <- zeroinfl(trip_count ~  
                           from_n_close_transport_scaled
                         + from_n_close_education_scaled
                         + from_n_close_health_scaled   
                         + from_n_close_leisure_scaled     
                         + to_n_close_leisure_scaled    
                         + to_n_close_transport_scaled  
                         + to_n_close_education_scaled  
                         + to_n_close_health_scaled     
                         + from_closest_station_dist_scaled 
                         + to_closest_station_dist_scaled        
                         + from_dist_city_center_scaled 
                         + to_dist_city_center_scaled            
                         + from_population_scaled 
                         + to_population_scaled                  
                         + route_time_scaled
                         + reachable_30                   
                         + loop_trip 
                         + weekday_cat
                         + from_perc_residential_scaled
                         + to_perc_residential_scaled
                         + from_perc_commercial_scaled
                         + to_perc_commercial_scaled
                         + from_perc_retail_scaled
                         + to_perc_retail_scaled
                         + from_perc_industrial_scaled
                         + to_perc_industrial_scaled 
                         + perc_cycleway_scaled | route_time,
                         data = trip_model,
                         dist = "negbin")


  #   zero-inflated negative binomial migrant added --------------------------------------------------------------------------------------------------------------
m_negbi0_migrant <- zeroinfl(trip_count ~  
                               from_n_close_transport_scaled
                             + from_n_close_shopping_scaled 
                             + from_n_close_education_scaled
                             + from_n_close_health_scaled   
                             + from_n_close_leisure_scaled     
                             + to_n_close_leisure_scaled    
                             + to_n_close_transport_scaled  
                             + to_n_close_shopping_scaled   
                             + to_n_close_education_scaled  
                             + to_n_close_health_scaled     
                             + from_closest_station_dist_scaled 
                             + to_closest_station_dist_scaled        
                             + from_dist_city_center_scaled 
                             + to_dist_city_center_scaled            
                             + from_population_scaled 
                             + to_population_scaled                  
                             + route_time_scaled
                             + reachable_30                   
                             + loop_trip 
                             + weekday_cat
                             + from_perc_residential_scaled
                             + to_perc_residential_scaled
                             + from_perc_commercial_scaled
                             + to_perc_commercial_scaled
                             + from_perc_retail_scaled
                             + to_perc_retail_scaled
                             + from_perc_industrial_scaled
                             + to_perc_industrial_scaled 
                             + citizen_notDE| route_time,
                             data = trip_model,
                             dist = "negbin")

# MODEL SELECTION ---------------------------------------------------------------------------------------
# put all models in list to compare with criteria

# simple models
models_result_basic <- list(m_negbi0_base, 
                            m_negbi0_pois,
                            m_negbi0_landuse)

# extended gravity
models_results_complex <- list(m_negbi0_all, 
                               m_negbi0_mig)


# with poisson
models_compare_negbi_zero_poi <- list(m_poi, 
                                      m_poi0, 
                                      m_negbi_all, 
                                      m_negbi0_all)


saveRDS(models_result_basic, "data/models_result_basic.rds")
saveRDS(models_results_complex, "data/models_results_complex")
saveRDS(models_compare_negbi_zero_poi, "data/models_compare_negbi_zero_poi.rds")


# function to create data frame with AIC, BIC McFadden's Pseudo R2, Cragg & Uhler Pseudo R2 and
# dispersion parameter
create_measure_df <- function(models) {
  model_measures <- data.frame(
    "AIC" = c(cbind(sapply(models, AIC))),
    "BIC" = c(cbind(sapply(models, BIC))),
    "R2McF" = c(cbind(sapply(models, function(x) {
      r2s <- pR2(x) # McFadden's
      mcf <- r2s[4]
      return(mcf)
    }))),
    "R2CU" = c(cbind(sapply(models, function(x) {
      r2s <- pR2(x)
      mcf <- r2s[5] # Cragg and Uhler
      return(mcf)
    }))),
    "disp" = c(cbind( # disperions
      sapply(models, function(x) {
        E2 <- resid(x, type = "pearson")
        p  <- length(coef(x))   
        disp <- sum(E2^2) / ( nrow(trip_model) - p)
        return(disp)
      }))))

return(model_measures)
}

# create data frames for both model list
models_result_basic_measures <- create_measure_df(models_result_basic)
models_results_complex_measures <- create_measure_df(models_results_complex)
models_compare_negbi_zero_poi <- create_measure_df(models_compare_negbi_zero_poi)


# save measure data frames
saveRDS(models_compare_negbi_zero_poi, "data/models_compare_negbi_zero_poi")
saveRDS(models_result_basic_measures, "data/models_result_basic_measures.rds")
saveRDS(models_results_complex_measures, "data/models_results_complex_measures.rds")



## plot residuals -----------------------------------------------------
# normal qq plot
## dummy nb is best, nb is always better than zero inflated
res <- residuals(m_negbi0_all, type="pearson")
plot(log(predict(m_negbi0_all)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

res <- residuals(m_negbi_all, type="pearson")
plot(log(predict(m_negbi_all)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

