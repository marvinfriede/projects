# coding: utf8

import random


def f(x, m, n):
	return x**m - n


def fPrime(x, m, n):
	return m * x**(m - 1)
	

def newton(m, n, guess = 0, tol = 1e-6, maxIter = 1e5):
	m = abs(m)
	n = abs(n)

	for i in range(int(maxIter)):
		try:
			nextGuess = guess - f(guess, m, n) / fPrime(guess, m, n)
		except ZeroDivisionError as e:
			continue
		
		if abs(guess - nextGuess) < tol:
			return nextGuess, i
		else:
			guess = nextGuess

	else:
		#print("Calculation exceeded maximum number of iterations!")
		#print("Using new guess and retrying...")
		return newton(m, n, random.randint(-n*100, n*100), tol, maxIter)


def main():
	print('Root Calculator\n---------------\n')

	try:
		n = float(input('number: '))
		m = float(input('n-th root: '))
	except Exception as e:
		print('Invalid input! Only numbers are possible.')
		exit(0)


	sol, iters = newton(m, n)
	sol = abs(sol)
	if m < 0:	sol = 1 / sol

	if n < 0:
		print('Solution: {:.10F}i'.format(sol))
	else:
		print('Solution: {:.10F}'.format(sol))
		

if __name__ == "__main__":
    main()