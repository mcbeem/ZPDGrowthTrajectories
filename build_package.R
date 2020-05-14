# build the package

# install packages
# install.packages("roxygen2")
# install.packages("devtools")
# install.packages("testthat")

library(devtools)
library(roxygen2)
library(testthat)
library(here)

setwd(here())

# create the package directory structure for the first time
#setwd("D:\\OneDrive")
#create("ZPDGrowthTrajectories")

# create the documentation
setwd(here())
document()

# install the package
setwd("..")
install("ZPDGrowthTrajectories")

#usethis::use_testthat()


#library(ZPDGrowthTrajectories)
#?ZPDGrowthTrajectories
#detach("package:ZPDGrowthTrajectories", unload=TRUE)

# run R CMD CHECK
setwd(here())
check()
devtools::test()

.rs.restartR()
detach("package:ZPDGrowthTrajectories", unload=TRUE)
install_github("mcbeem/ZPDGrowthTrajectories")
library(ZPDGrowthTrajectories)
?visualizeSchool

      library(devtools)
      library(roxygen2)
      library(testthat)

      setwd("/Users/matt/OneDrive/giftedCalcs")
      document()

      setwd("..")
      install("giftedCalcs")

      setwd("/Users/matt/OneDrive/giftedCalcs")
      check()
      devtools::test()

      test(filter="var_mean")
      test(filter="r_identified")
      test(filter="d_identified")
      test(filter="d_identified_v")
      test(filter="q_identified")
      test(filter="p_identified")
      test(filter="mean_identified")
      test(filter="sd_identified")
      test(filter="marginal_psychometrics")
      test(filter="conditional_moments")
      test(filter="conditional_p_id")
      test(filter="estimate_valid")
      test(filter="boot_estimate_valid")
      test(filter="estimate_performance")










      setwd("..")
      install("giftedCalcs")


      detach("package:giftedCalcs", unload=T)
      remove.packages("giftedCalcs")

      devtools::install_github("mcbeem/giftedCalcs", force=T)
      library(giftedCalcs)
      help(package="giftedCalcs")


