# pip install git+https://github.com/nficano/pytube

import os
import re
from pytube import YouTube

destination_path = "/home/marvin/Musik"


def doDownload(url):
  print("Retrieving video...")
  try:
    yt = YouTube(url)
    audio = yt.streams.get_audio_only()
    # printInfo(yt)

    title, oldName, newName = rename(yt)

    print("Downloading...")
    audio.download(destination_path, title)
    os.rename(oldName, newName)
    print("Download completed. (path: {})".format(newName))
  except FileNotFoundError as e:
    print("FileError: {}".format(e))
    return
  except Exception as e:
    print("Error: {}".format(e))
    return


def rename(yt):
  title = yt.title.replace("'", "")  # remove apostroph (breaks renaming)
  title = re.sub(r"[\(\[].*?[\)\]]", "", title)  # remove (...) and [...]
  title = title.rstrip()  # remove trailing white spaces
  title = title.title()  # capitalize only first letter
  oldName = destination_path + "/" + title + ".mp4"
  newName = destination_path + "/" + title + ".mp3"
  return title, oldName, newName


def printInfo(yt):
  print("Title: ", yt.title)
  print("Number of views: ", yt.views)
  print("Length of video: ", yt.length, "seconds")
  print("Description: ", yt.description)
  print("Ratings: ", yt.rating)


def main():
  # get urls for download from "urls.txt"
  scriptPath = os.path.dirname(__file__)
  fileName = "urls.txt"

  # open file, read lines and close
  f = open(os.path.join(scriptPath, fileName), "r")
  for url in f.readlines():
    doDownload(url)
    print("")
  f.close()


if __name__ == "__main__":
  main()
