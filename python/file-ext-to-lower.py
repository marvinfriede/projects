#!/bin/env python3

import os


def fileExtToLower(folder):
  for fname in os.listdir(folder):
    name, ext = os.path.splitext(fname)
    os.rename(os.path.join(folder, fname), os.path.join(folder, name + ext.lower()))


def main():
  fileExtToLower(".")


if __name__ == "__main__":
  main()
