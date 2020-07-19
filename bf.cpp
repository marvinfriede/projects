#include <iostream>
#include <iomanip>
#include <string>
#include <chrono>
#include <math.h>

// without the most uncommon letters: q, x, j, y, v
static const std::string alphabet = "0123456789abdefghiklmnoprstuABDEFGHIKLMNOPRSTU";
//const std::string alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const int alphabetSize = alphabet.length();


// converting number to string from alphabet
std::string stringify(unsigned long long val) { 
	std::string ret;
	while (val) { 
		ret += alphabet[val % alphabetSize];
		val /= alphabetSize;
	}
	return ret;
}


int main() {
	static const std::string pwd = "HasileiN1";
	static const int pwdSize = pwd.length();
	std::string candidate = "";

	// calculate number of possible passwords with that length and number of chars
	static const unsigned long long maxIter = pow(alphabetSize, pwdSize); // 9999999999999ULL;
	static const unsigned long long startIter = pow(alphabetSize, pwdSize - 1);
	std::cout << "Trying to find password out of " << maxIter - startIter;
	std::cout << " possibilities...." << std::endl;
	
	

	// start timer
	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();


	for (unsigned long long i = startIter; i < maxIter; i++) {
    candidate = stringify(i);
		//std::cout << candidate << " thread: " << omp_get_thread_num() << "\n";
    if (pwd.compare(candidate) == 0) {
			std::cout << "\nPassword cracked! You entered: '";
			std::cout << candidate << "'." << std::endl;

			std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
			double time = std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count();
			std::cout << std::fixed << std::setprecision(2);
			std::cout << "Time ellapsed: ";
			std::cout << time / 1000000.0 << "s = ";
			std::cout << time / 60000000.0 << "min";
			std::cout << std::endl;

			std::cout << "\n-----------------------------------" << std::endl;
			std::cout << "Press Ctrl+C to close the programm." << std::endl;
		}
	}

 
  return 0;
}

// compiled with: g++ -fopenmp -std=c++11 brute-force.cpp -o brute-force.exe -O3