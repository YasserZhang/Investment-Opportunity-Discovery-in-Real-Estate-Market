## Investment-Opportunity-Discovery-in-Real-Estate-Market

Programming language: R
Operating system: windows 10

#### Brief introduction: the codes involve the final training, testing, and visulization sessions. Codes for cleanining and preprocessing sessions are not included because the raw data are too big to upload (around 30 gb).

#### Recomended IDE: RStudio

#### codes execution order

	-first, set the working directory under the main folder "final project"

	-second, execute "EMSteps.R", which implements modeling training and also draws at the end a 	plot of the Kendall's Tau Coeffiecnts againt iterations.

	-third, execute "Mapping_Prediction.R", which reproduces Figure 3 and 4 in the paper.

	-fourth, execute "testingAndApplication.R" (which must be executed only after "EMSteps.R" has 	been executed), which performs validation on testing data, and apply the model to rental 	properties. The code file reproduces Figure 7 and 8.
