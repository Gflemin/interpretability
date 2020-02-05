
### Get here/pacman and source scripts

# here
library(here)

# pacman
library(pacman)

# source scripts
source(here::here("libraries.R"))
source(here::here("functions.R"))

# load up our drake plan
source(here::here("drake_plan.R"))

# run our drake plan
make(plan)
plan
# visualize our drake plan as a network graph
vis_drake_graph(config)

# load and view our local interpretability plots
loadd(local_plots)
local_plots

# pdps
loadd(c(trained_learners, data_clean))

