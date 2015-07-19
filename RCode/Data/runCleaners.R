####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

# Get list of cleaning scripts
cleanSripts = paste0(pathCode, '/Data/getData/') %>% list.files(.) %>% .[substrRight(.,2)=='.R'] %>% paste0(paste0(pathCode, '/Data/getData/'), .)

# Except for getdistmat, because getdistmat is parallelized itself
cleanSripts = cleanSripts[!grepl('getDistMat.R',cleanSripts)]

# Parameters for parallelization
cl = makeCluster(8)
registerDoParallel(cl)

# Run cleaning scripts in paralle
foreach(script = cleanSripts) %dopar% { source(script) }

# Free my clusters
stopCluster(cl)