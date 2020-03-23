library(tidyverse)
library(cowplot)

#read in all the sites (This is xanthes file which has the actual site id i need for ak)
df <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_SynthesisData_07032019_XJW_CMDUpdate.csv') %>% 
                filter(treatment != 'Control') %>%
                dplyr::select(id, site, project.id, project.name, above.carbon.combusted, below.ground.carbon.combusted)

#change C from g to kg
df <- df %>% mutate(above.carbon.combusted = above.carbon.combusted / 1000, below.ground.carbon.combusted = below.ground.carbon.combusted / 1000)

#read in sk SE, join to df by id
sk_error <- read_csv("/mnt/data1/boreal/spotter/combustion/catherine_stand_age_11_25_19/SP_DATA_ARCHIVE.csv") %>% 
  dplyr::select(ID, AB.Combusted.C.SD, BG.Combusted.C.SE) %>%
  drop_na()
sk_error <- sk_error %>% dplyr::rename(AB.Combusted.C.SE = AB.Combusted.C.SD, id = ID)

#merge sk_error to df
sk_error <- left_join(sk_error, df, by = 'id') %>% dplyr::select(id, AB.Combusted.C.SE, BG.Combusted.C.SE)

#read in ak SE, join to df by 
ak_error <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Uncertainty_GillesCreekPlots.csv')
ak_error <- ak_error %>% dplyr::rename(AB.Combusted.C.SE = Unc_above, BG.Combusted.C.SE = Unc_below, site = X1)


#get the subset of the intial file which matches ak_error
sub <- df %>% filter(project.name == 'AK_Rogers')

#I want to join the last 1-2 numbers in sub$site to ak_error$site
#remove AK_Rogers_PO
sub <- sub %>% mutate(site = str_replace(site, 'AK_Rogers_P0', ''))

#where project.id == 'BR_AK1.sf.30' I need to change id to sf.713 to match a previous change I made
sub <- sub %>% mutate(id = ifelse((id == 'sf.712' & project.id == 'BR_AK1.sf.30'), 'sf.713', id))

#remove the last 0
sub <- sub %>% mutate(site = str_replace(site, '0', '')) %>% mutate(site = as.numeric(site)) %>% dplyr::select(id, site)

#join ak_error to sub
ak_error <- left_join(sub, ak_error, by = 'site') %>% dplyr::select(-site)


#read in nwt SE, here nwt_main plot.name joins to nwt_se plot_name_full, nwt_main plot.name joins to df$site with some fixing
nwt_main <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/NWT_spatial_data_modv3.csv') %>%
                    select(site, plot.name, soil.carbon.cons.gCm2, tree.bio.cons.m2B)
nwt_se <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/nwt_SE_below.csv') %>% 
                   rename(plot.name = plot_name_full) %>%
                   select(plot.name, std.error.soil.comb)

nwt_error <- left_join(nwt_se, nwt_main, by = 'plot.name')

#now readjusted the nwt_main plot.name to match df$site
df2 <- df %>% filter(project.name == 'NWT_MackWalker')

#remove some zeros which are mismatched in nwt_error to df2
nwt_error <- nwt_error %>% mutate(plot.name = str_replace(plot.name, 'SS0', 'SS'))
nwt_error <- nwt_error %>% mutate(plot.name = str_replace(plot.name, 'ZF0', 'ZF'))
nwt_error <- nwt_error %>% mutate(plot.name = str_replace(plot.name, '-0', '-'))
nwt_error <- nwt_error %>% mutate(plot.name = str_replace(plot.name, '-0', '-'))

#rename plot.name to site
nwt_error <- nwt_error %>% select(-site) %>% rename(site = plot.name)

#change g to k/C
nwt_error <- nwt_error %>% mutate_if(is.numeric, funs(. /1000))

#remove the prefix in df2
df2 <- df2 %>% mutate(site = str_replace(site, 'NWT_MW_', ''))


#make a plot of se vs observed C for above and below, no aboveground in NWT so make NA
#join nwt_error to the main df by linking x to id
nwt_error2 <- left_join(nwt_error, df2 %>% dplyr::select(id, site), by = 'site')
nwt_error2 <- nwt_error2 %>% dplyr::select(id, std.error.soil.comb) %>% 
  dplyr::rename(BG.Combusted.C.SE = std.error.soil.comb) %>%
  mutate(AB.Combusted.C.SE = NA, project = 'NWT')

#add project names to ak and sk
ak_error <- ak_error %>% mutate(project = 'AK')
sk_error <- sk_error %>% mutate(project = 'SK')
nwt_error2 <- nwt_error2 %>% select(names(ak_error))

#combine the errors
all_error <- rbind(ak_error, sk_error, nwt_error2)

#join the error to original df
all_error <- left_join(all_error, df %>% dplyr::select(id, above.carbon.combusted, below.ground.carbon.combusted), by = 'id')

#get ony aboveground error, NA removed
all_above_error <- all_error %>% select(id, project, above.carbon.combusted, AB.Combusted.C.SE) %>% drop_na()

#get only belowground error, NA removed
all_below_error <- all_error %>% select(id, project, below.ground.carbon.combusted, BG.Combusted.C.SE) %>% drop_na()
# colSums(is.na(all_below_error))

#plot observed C vs SE for above
p1 <- ggplot(all_above_error, aes(x = above.carbon.combusted, y =  AB.Combusted.C.SE, color = project)) + 
  geom_point() + geom_jitter() + 
  scale_color_manual(values=c("indianred2", "lightskyblue2")) +
  labs(x = Measured ~ (kg ~C/m^2), y = SE ~ (kg ~C/m^2)) +  
  xlim(0, 2) + ylim(0, 2) + 
  theme_bw() +
  ggtitle('Aboveground') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none") +
  theme(text=element_text(size=18))

p2 <- ggplot(all_below_error, aes(x = below.ground.carbon.combusted, y =  BG.Combusted.C.SE, color = project)) + 
  geom_point() + geom_jitter() + 
  scale_color_manual(values=c("indianred2", "seagreen2", "lightskyblue2")) +
  labs(x = Measured ~ (kg ~C/m^2), y = '') +  
  xlim(0, 10) + ylim(0, 10) + 
  theme_bw() +
  ggtitle('Belowground') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title=element_blank()) + 
  theme(legend.position = c(0.8, 0.8)) +
  theme(text=element_text(size=18))

final <- plot_grid(p1, p2, ncol = 2) +
  ggsave(filename = "/mnt/data1/boreal/spotter/combustion/final_files/plots/observed_vs_se.png", device = 'png', dpi = 150, width = 10, height = 5)

#--------------------------------------------run the monte carlo on alaska
#join the original C vaues from ak back into ak_error
ak_sub <- df %>% dplyr::filter(project.name == 'AK_Rogers') %>%
          mutate(id = ifelse((id == 'sf.712' & project.id == 'BR_AK1.sf.30'), 'sf.713', id)) %>%
          dplyr::select(id, above.carbon.combusted, below.ground.carbon.combusted)
         

ak_error <- left_join(ak_error, ak_sub, by = 'id')

#start monte carlo to get new C estimates
NumSims = 1000 #number of Monte Carlo simulations

#list to store all the dataframes
final_ak <- list()
#start the iteration
for (counter_MC in 1:NumSims) {
  
  print(paste0("AK Iter = ", counter_MC))
   #maniuplate the error rates within a normal distribuation
  a <- rnorm(1, 0,1)
  b <- rnorm(1, 0,1)
  
  ak_error_sub <- ak_error %>% mutate(AB.Combusted.C.SE = AB.Combusted.C.SE * a, BG.Combusted.C.SE = BG.Combusted.C.SE * b)
  
  #add this new error back to observed C
  ak_error_sub <- ak_error_sub %>% 
    mutate(above.carbon.combusted = above.carbon.combusted + AB.Combusted.C.SE, 
           below.ground.carbon.combusted = below.ground.carbon.combusted + BG.Combusted.C.SE, 
           Iter = counter_MC) %>%
    select(id, Iter, above.carbon.combusted, below.ground.carbon.combusted)

  final_ak[[length(final_ak) + 1]] <- ak_error_sub
}
  
#bind the rows and join to original data
final_ak <- bind_rows(final_ak)
final_ak <- left_join(final_ak, ak_sub %>% dplyr::select(id), by = 'id') %>%
            select(id, Iter, above.carbon.combusted, below.ground.carbon.combusted)

#--------------------------------------------run the monte carlo on SK
#join the original C vaues from ak back into ak_error
sk_sub <- df %>% dplyr::filter(project.name == 'SK_RTJ') %>%
  dplyr::select(id, above.carbon.combusted, below.ground.carbon.combusted)


sk_error <- left_join(sk_error, sk_sub, by = 'id')

#start monte carlo to get new C estimates
NumSims = 1000 #number of Monte Carlo simulations

#list to store all the dataframes
final_sk <- list()
#start the iteration
for (counter_MC in 1:NumSims) {
  
  print(paste0("SK Iter = ", counter_MC))
  #maniuplate the error rates within a normal distribuation
  a <- rnorm(1, 0,1)
  b <- rnorm(1, 0,1)
  
  sk_error_sub <- sk_error %>% mutate(AB.Combusted.C.SE = AB.Combusted.C.SE * a, BG.Combusted.C.SE = BG.Combusted.C.SE * b)
  
  #add this new error back to observed C
  sk_error_sub <- sk_error_sub %>% 
    mutate(above.carbon.combusted = above.carbon.combusted + AB.Combusted.C.SE, 
           below.ground.carbon.combusted = below.ground.carbon.combusted + BG.Combusted.C.SE, 
           Iter = counter_MC) %>%
    select(id, Iter, above.carbon.combusted, below.ground.carbon.combusted)
  
  final_sk[[length(final_sk) + 1]] <- sk_error_sub
}

#bind the rows and join to original data
final_sk <- bind_rows(final_sk)
final_sk <- left_join(final_sk, sk_sub %>% dplyr::select(id), by = 'id') %>%
  select(id, Iter, above.carbon.combusted, below.ground.carbon.combusted)

#--------------------------------------------run the monte carlo on NWT
NumSims = 1000 #number of Monte Carlo simulations

#list to store all the dataframes
final_nwt <- list()
#start the iteration
for (counter_MC in 1:NumSims) {
  
  print(paste0("NWT Iter = ", counter_MC))
  
  #random bias terms
  This_pctC = rnorm(1,50,3) #percent C varied with 3% stdev, apply systematically within every Monte C sim
  This_tree_bias = rnorm(1,1,0.2)
  This_soil_bias = rnorm(1,0,1)

  #for each site make new values for soil and tree C combusted
  nwt_error_sub <- nwt_error %>% mutate(soil_comb_temp = soil.carbon.cons.gCm2 + This_soil_bias*std.error.soil.comb)
  nwt_error_sub <- nwt_error_sub %>% mutate(tree_comb_temp = tree.bio.cons.m2B * This_pctC/100 * This_tree_bias)
  nwt_error_sub <- nwt_error_sub %>% mutate(Iter = counter_MC)
  
  #change some names and select some columns
  nwt_error_sub <- nwt_error_sub %>% 
    rename(below.ground.carbon.combusted = soil_comb_temp, above.carbon.combusted = tree_comb_temp) %>%
    select(site, Iter, above.carbon.combusted, below.ground.carbon.combusted)
  
  #add to the list
  final_nwt[[length(final_nwt) + 1]] <- nwt_error_sub

}

final_nwt <- bind_rows(final_nwt)

##join to id from the original data
final_nwt <- left_join(final_nwt, df2 %>% dplyr::select(id, site), by = 'site') %>% 
             select(id, Iter, above.carbon.combusted, below.ground.carbon.combusted)

#join all the monte carlos back together
all_monte <- rbind(final_ak, final_sk, final_nwt)

#determine which ids in original df will need to be filled with covariance 
df <- read_csv("/mnt/data1/boreal/spotter/combustion/final_files/raw/all_predictors.csv")

#get some columns to remove such as id, latitude etc.
bad_cols <- c('project.name', 'burn_year', 'latitude', 'longitude')

#remove abovground bad cols
above <- df %>% dplyr::select(-(bad_cols), -below.ground.carbon.combusted) %>% drop_na()  
above$above.carbon.combusted <- above$above.carbon.combusted/ 1000.0

#remove belowground bad cols
below <- df %>% dplyr::select(-(bad_cols), -above.carbon.combusted) %>% drop_na()  
below$below.ground.carbon.combusted <- below$below.ground.carbon.combusted / 1000.0

#get above fill
above_fill <- above %>% filter(! id %in% unique(all_monte$id))

#get below fill
below_fill <- below %>% filter(!id %in% unique(all_monte$id))

#fill with appropriate covariance

#create a final df used for new monte carlo predictions


##----------notes
# for missing sites multiply observed C by mean cv for SE per site, above and below, apply SE same way 
# as SK

#what to do with negative values? Make 0?
