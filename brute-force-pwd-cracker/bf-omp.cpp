// compiled with: g++ -fopenmp -std=c++11 bf-omp.cpp -o bf-omp.exe -O3

#include <iostream>
#include <iomanip>
#include <string>
#include <chrono>
#include <math.h>
#include <omp.h>

// [a-zA-Z0-9]
static const std::string alphabetFull = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

// [a-zA-Z0-9] without the most uncommon letters: q, x, j, y, v
static const std::string alphabet = "0123456789abdefghiklmnoprstuABDEFGHIKLMNOPRSTU";

// number of chars in test sample
static const int alphabetSize = alphabet.length();

// converting number to string from alphabet
std::string stringify(unsigned long long val)
{
  std::string ret;
  while (val)
  {
    ret += alphabet[val % alphabetSize];
    val /= alphabetSize;
  }
  return ret;
}

int main()
{
  static const std::string pwd = "tesTpwd1";
  static const int pwdSize = pwd.length();
  std::string candidate = "";

  // calculate number of possible passwords with that length and number of chars
  static const unsigned long long maxIter = pow(alphabetSize, pwdSize); // 9999999999999ULL;
  static const unsigned long long startIter = pow(alphabetSize, pwdSize - 1);
  std::cout << "Trying to find password out of " << maxIter - startIter;
  std::cout << " possibilities...." << std::endl;

  // start timer
  std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

  omp_set_dynamic(0);
  omp_set_num_threads(4);
#pragma omp parallel for firstprivate(candidate) schedule(dynamic)
  for (unsigned long long i = startIter; i < maxIter; i++)
  {
    candidate = stringify(i);
    //std::cout << candidate << " thread: " << omp_get_thread_num() << "\n";
    if (pwd.compare(candidate) == 0)
    {
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