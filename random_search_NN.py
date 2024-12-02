import random
import sys

for i in range(0, 1000):

	f = open(sys.argv[2] +  "_" +  str(i) + ".py", "w")

	f.write("import sklearn\n")
	f.write("import pandas as pd\n")
	f.write("import numpy as np\n")
	f.write("import math\n")
	f.write("import statistics\n")
	f.write("from sklearn.neural_network import MLPRegressor\n")
	f.write("from sklearn.preprocessing import MinMaxScaler\n")
	f.write("from sklearn.metrics import r2_score\n\n")
	f.write("from sklearn.model_selection import cross_validate, train_test_split\n")

	f.write('d = pd.read_csv("' + sys.argv[1] + '", sep="\\t")\n')
	
	f.write("try:\n")
	f.write('\td = d.drop(["NeverVsCurrent", "NeverVsCurrent_x_PRS"], axis = 1)\n')
	f.write("except:\n")
	f.write('\tprint("NeverVsCurrent not in d")\n')

	f.write("d = d.dropna()\n")
	#f.write('y_train = d["logBMI"]\n')
	f.write('X = d.drop(["BMI", "logBMI", "IID"], axis = 1)\n')
	f.write('scaler = MinMaxScaler()\n')
	f.write('X_noInt = X[X.columns.drop(list(X.filter(regex="_x_PRS")))]\n')
	f.write('X_train =  scaler.fit_transform(X_noInt)\n')

	hidden_layer_sizes = random.randint(10, 200)
	learning_rate_init = random.uniform(.01, .0001)
	learning_rate = random.choice(["constant", "invscaling"])
	power_t = random.uniform(.4, .6)
	momentum = random.uniform(.8, 1.0)
	batch_size = random.randint(32, 256)
	hidden_layers = random.randint(1, 2)
	pheno = random.choice(["BMI", "logBMI"])
	
	if pheno == "BMI":
		f.write('y_train = d["BMI"]\n')
	elif pheno == "logBMI":
		f.write('y_train = d["logBMI"]\n')

	hls = str(hidden_layer_sizes)
	if hidden_layers == 2:
		hls = str(hidden_layer_sizes) + "," + str(hidden_layer_sizes)

	f.write('nn = MLPRegressor(hidden_layer_sizes=(' + hls + ',), solver="sgd", learning_rate="' + str(learning_rate) + '", power_t=' + str(power_t) + ', momentum=' + str(momentum) + ', batch_size=' + str(batch_size) + ', max_iter=1000)\n')

	f.write('nn_scores = cross_validate(nn, X_train, y_train, scoring="r2", return_estimator=True, cv=10)\n')

	f.write("\n")	
	f.write('print("phenotype\\thidden_layer_sizes\\thidden_layers\\tlearning_rate_init\\tlearning_rate\\tpower_t\\tmomentum\\tbatch_size\\tR2")\n')
	f.write('print("' + pheno + "\\t" + str(hidden_layer_sizes) + "\\t" + str(hidden_layers) + "\\t" + str(learning_rate_init) + "\\t" + str(learning_rate) + "\\t" + str(power_t) + "\\t" + str(momentum) + "\\t" + str(batch_size) + '\\t" + ' + "str(nn_scores['"'test_score'"'].mean())" + ")")

	f.close()
