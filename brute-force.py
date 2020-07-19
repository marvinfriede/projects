import itertools
import time

def crackerAllLengths(maxChars, alphabet, userPwd):
	for charLength in range(maxChars + 1):
		print("Trying password with {} characters".format(charLength))
		passwords = itertools.product(alphabet, repeat = charLength)
		for i in passwords:
			pwdGuess = "".join(i)
			if (pwdGuess == userPwd):
				print("\n\nPassword cracked! Your password is '{}'.".format(pwdGuess))
				return

	print("\n\nPassword was not found within possible characters.")
	return


def cracker(pwdLength, alphabet, userPwd):
	print("Starting to brute force your {} character password...".format(pwdLength))

	passwords = itertools.product(alphabet, repeat = pwdLength)
	for i in passwords:
		pwdGuess = "".join(i)
		if (pwdGuess == userPwd):
			print("\n\nPassword cracked! Your password is '{}'.".format(pwdGuess))
			return

	print("\n\nPassword was not found within possible characters.")
	return


def main():
	alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
	alphabet_modified = "abcdefghiklmnoprstuvwzABCDEFGHIKLMNOPRSTUVZ1234567890"
	userPwd = "AffeN,,12"
	userPwd2 = "HasileiN1"
	maxChars = len(userPwd)

	start_time = time.time()
	cracker(maxChars, alphabet_modified, userPwd2)
	exec_time = round((time.time() - start_time), 2)
	print("Time elapsed: ({:.2f}s)".format(exec_time))

	return


if __name__ == '__main__':
	main()