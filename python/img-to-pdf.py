#!/bin/env python3

import sys
from datetime import datetime
from glob import glob
from PIL import Image


fileName = "FriedeMarvin" + datetime.now().strftime("%Y%M%d") + ".pdf"


def main():
	# only one file given
	if len(sys.argv) == 2:
		img1 = Image.open(sys.argv[1])
		img1.save(fileName, "PDF", resolution=100.0)
		return


	# no files given
	if len(sys.argv) == 1:
		imageFiles = glob("*.jpg") + glob("*.jpeg") + glob("*.png")
	# multiple files given
	elif len(sys.argv) > 2:
		imageFiles = sys.argv[1:]

	# safety check
	if len(imageFiles) == 0:
		echo("No files found.")
		return
	
	# actual conversion
	img1 = Image.open(imageFiles[0])
	images = []
	for file in imageFiles[1:]:
		images.append(Image.open(file))

	img1.save(fileName, "PDF", resolution=100.0, save_all=True, append_images=images)



if __name__ == '__main__':
	main()
