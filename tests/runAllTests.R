
library(testthat)
print("running all tests")
test_check("MicroPlate") # this works with check() it does not work by ctrl+enter :(

# testPath=paste(getwd(),"/tests/testthat/",sep="")
# codePath=paste(getwd(),"/R/",sep="")
# auto_test(test_path=testPath, code_path=codePath)
# test_dir(testPath)# does not work..
