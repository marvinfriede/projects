// compiled with: g++ -fopenmp -std=c++11 bf-omp.cpp -o bf-omp.exe -O3

#include <stdio.h>
#include <iostream>
#include <string>
#include <chrono>
#include <math.h>
#include <omp.h>

// [a-zA-Z0-9]
static const std::string alphabetFull = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.,-_!?";

// [a-zA-Z0-9] without the most uncommon letters: q, x, j, y, v
static const std::string alphabet = "0123456789abdefghiklmnoprstuABDEFGHIKLMNOPRSTU";

// number of chars in test sample
static const int alphabetSize = alphabetFull.length();



// converting number to string from alphabet
std::string stringify(unsigned long long val)
{
  std::string ret;
  while (val)
  {
    ret += alphabetFull[val % alphabetSize];
    val /= alphabetSize;
  }
  return ret;
}

int main()
{
  // disable buffering to print immediately to sdtout
	setvbuf (stdout, NULL, _IONBF, 0); 

  static const std::string pwd = "tesT?d1";
  static const int pwdSize = pwd.length();
  std::string candidate = "";

  // calculate number of possible passwords with that length and number of chars
  static const unsigned long long maxIter = pow(alphabetSize, pwdSize); // 9999999999999ULL;
  static const unsigned long long startIter = pow(alphabetSize, pwdSize - 1);
  printf("Trying to find password out of %E possibilities...", (double)(maxIter - startIter));

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
      std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
      double time = std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count();

      printf("\nPassword cracked! You entered: %s", candidate.c_str());
      printf("\nTime ellapsed: %.2f s = %.2f min \n", time / 1000000.0, time / 60000000.0);
      printf("\n-----------------------------------");
      printf("\nPress Ctrl+C to close the programm.");
    }
  }

  return 0;
}