# this contains a reference class or R5
# this would make R a functional programing languange.
# unfortunalty it is not supported by roxygen
# and thus you cannot document and export it properly
# this results in the class generator and the class methods not being exported 
# AKA used outside of the package
# AKA working
#
# FUCK!!!


#' class to test reference class
#' @export
MyClass <- setRefClass(
  "MyClass",
  fields = list(
    x = "ANY",
    y = "numeric",
    z = "character"
  ),
  methods = list(
    initialize = function(x = NULL, y = 1:10, z = letters)
    {
      "This method is called when you create an instance of the class."
      x <<- x
      y <<- y
      z <<- z
      print("You initialized MyClass!")
    },
    hello = function()
    {
      "This method returns the string 'hello'."
      "hello"
    },
    doubleY = function()
    {
      2 * y
    },
    printInput = function(input)
    {
      if(missing(input)) stop("You must provide some input.")
      print(input)
    }
  )
)

# 
# obj1 <- MyClass$new()
# obj1$hello()
# obj1$doubleY()
# 
# obj2 <- MyClass$new(x = TRUE, z = "ZZZ")
# obj2$printInput("I'm printing a line!")
# 
# test=new("MyClass")
# 
# 



