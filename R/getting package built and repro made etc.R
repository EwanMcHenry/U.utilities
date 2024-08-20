
library(devtools)
setwd("")

test("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\Analysis\\U.utilities")
document("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\Analysis\\U.utilities")
build("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\Analysis\\U.utilities")
install("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\Analysis\\U.utilities")
check("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\Analysis\\U.utilities")

library(U.utilities)
pad.lim(1:5)




# bash
#git remote add origin https://github.com/EwanMcHenry/U.utilities.git


# R
gitcreds::gitcreds_set()


devtools::install_github("EwanMcHenry/U.utilities")

