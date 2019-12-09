Pipeline:
- parse input text, create program description, save: 
  + source locations
  + annotations controlling cps-translation
  + names of bound variables
- transform to A-normal form
  + have to generate new bindings
- perform control flow analysis
  + requires uniquely named variables, transform program into initial
    environment and main function
  + requires expressions with accessible variables (both bound and free)
  + have to keep set of visited states -- so states should be simple and
    first-order
  + maybe keep a map from labels to subexpressions and states can contain
    labels
- perform defunctionalization using information gained from previous
  transformation
