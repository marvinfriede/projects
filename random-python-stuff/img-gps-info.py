from PIL.ExifTags import TAGS, GPSTAGS
from PIL import Image


image = 'C:\\Users\\Marv\\Desktop\\img-metadata\\Amaze group - 09.09.2018 - Tomb 1.jpg'


def get_exif(filename):
    image = Image.open(filename)
    image.verify()
    return image._getexif()


def get_labeled_exif(exif):
    labeled = {}
    for (key, val) in exif.items():
        labeled[TAGS.get(key)] = val

    return labeled


def get_geotagging(exif):
    if not exif:
        raise ValueError("No EXIF metadata found")

    geotagging = {}
    for (idx, tag) in TAGS.items():
        if tag == 'GPSInfo':
            if idx not in exif:
                raise ValueError("No EXIF geotagging found")

            for (key, val) in GPSTAGS.items():
                if key in exif[idx]:
                    geotagging[val] = exif[idx][key]

    return geotagging


def get_decimal_from_dms(dms, ref):

    degrees = dms[0][0] / dms[0][1]
    minutes = dms[1][0] / dms[1][1] / 60.0
    seconds = dms[2][0] / dms[2][1] / 3600.0

    if ref in ['S', 'W']:
        degrees = -degrees
        minutes = -minutes
        seconds = -seconds

    return round(degrees + minutes + seconds, 5)


def get_coordinates(geotags):
	try:
		lat = get_decimal_from_dms(geotags['GPSLatitude'], geotags['GPSLatitudeRef'])
		lon = get_decimal_from_dms(geotags['GPSLongitude'], geotags['GPSLongitudeRef'])
		return (lat,lon)
	except:
		return "No Coordinates given!"


def main():
	exif = get_exif(image)
	geotags = get_geotagging(exif)
	print(get_coordinates(geotags)))


if __name__ == "__main__":
    main()
