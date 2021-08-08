#!/bin/env python3
# coding: utf8

def f(x):
	return x**2-2


def fPrime(x):
	return 2*x
	

def newton(f, fPrime, guess = 0, tol = 1e-6, maxIter = 1e5):
	for i in range(int(maxIter)):
		try:
			nextGuess = guess - f(guess) / fPrime(guess)
		except ZeroDivisionError as e:
			continue
		
		if abs(guess - nextGuess) < tol:
			return nextGuess, i
		else:
			guess = nextGuess

	else:
		print("Calculation exceeded maximum number of iterations!")
		print("Changing guess (+1000) and retrying...")
		return newton(f, fPrime, guess+1000)



def main():
	root, nIter = newton(f, fPrime)
	print("The root {:.4F} was found after {} iterations.".format(root,nIter))


if __name__ == "__main__":
    main()