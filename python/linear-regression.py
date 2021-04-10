# coding: utf-8 

import numpy as np


def linear(x, y, n):
	n = len(x)
	b = (n*sum(x*y)-sum(x)*sum(y))/(n*sum(x*x)-sum(x)**2)
	a = (sum(y)-b*sum(x))/n
	
	return a, b


def R2(x, y, n, a, b, c = 0):
	yMean = sum(y) / n
	yHat = a + b * x + c * x*x

	sst = sum((y - yMean) ** 2)
	sse = sum((yHat - yMean) ** 2)
	ssr = sum((y - yHat) ** 2)

	r2 = sse / sst
	r2_adj = 1 - (n - 1) / (n - 2) * (1 - r2)

	return r2 * 100 , r2_adj * 100


def main():
	#data = np.genfromtxt('out.txt', dtype=float, delimiter=None)
	#t = data[:,0]
	#a = data[:,1]
	
	x = np.array([0.0162, 1.4094, 3.0132, 5.5080, 8.1000, 10.3032, 11.8422])
	y = np.array([0.0089, 0.0265, 0.0400, 0.0650, 0.0835, 0.1017, 0.1092])

	if len(x) == len(y):
		n = len(x)
	else:
		exit("Number of data points for x and y are different!")



	a, b = linear(x, y, n)
	r2, r2_adj = R2(x, y, n, a, b)

	print("Linear Regression")
	print("-----------------")
	print("")
	print("y = {:.10f} + {:.10f} x".format(a, b))
	print("")
	print("R² = {:.2f}%".format(r2))
	print("R²_adj = {:.2f}%".format(r2_adj))


if __name__ == '__main__':
	main()