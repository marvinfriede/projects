#!/bin/env python3

from PIL import Image, ExifTags
import glob
import argparse


filepath = "~/Bilder"

# argparser
parser = argparse.ArgumentParser()
parser.add_argument("-f", "--full", help="Print all ExifTags", action="store_true")
flags = parser.parse_args()



def getImage(images):
	if len(images) != 0:
		for image in images:
			print("\n--------------------------------------------------------------\n")
			print("Image: {}\n".format(image))
			img = Image.open(filepath + str(image))
			getMetadata(img)


def getMetadata(img):
	global filepath, flags
	for i, j in img._getexif().items():
		if i in ExifTags.TAGS:
			if flags.full:
				print(ExifTags.TAGS[i] + ": " + str(j))
			else:
				chosenTags = ["Model", "DateTime", "DateTimeOriginal", "Software", "CameraOwnerName", "GPSInfo", "Make"]
				if ExifTags.TAGS[i] in chosenTags:
					print(ExifTags.TAGS[i] + ": " + str(j)) #wie heißt der i-te ExifTag


def main():
	jpgImages = glob.glob('*.jpg')
	jpegImages = glob.glob('*.jpeg')
	pngImages = glob.glob('*.png')

	getImage(jpgImages)
	getImage(jpegImages)
	getImage(pngImages)


if __name__ == "__main__":
    main()

