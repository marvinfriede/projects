# pip install pytube3
# pip install git+https://gitlab.com/obuilds/public/pytube@ob-v3 --upgrade

import os
import re
from pytube import YouTube


def doDownload(url):
  print("Retrieving video...")
  try:
    yt = YouTube(url)
  except:
    print("{} is not a valid url".format(url))
    return

  path = "/home/marvin/Musik"
  title = yt.title.replace("'", "")  # remove apostroph (breaks renaming)
  title = re.sub(r"[\(\[].*?[\)\]]", "", title)  # remove (...) and [...]
  title = title.rstrip()  # remove trailing white spaces
  oldName = path + "/" + title + ".mp4"
  newName = path + "/" + title + ".mp3"

  # print("Title: ", yt.title)
  # print("Number of views: ", yt.views)
  # print("Length of video: ", yt.length, "seconds")
  # print("Description: ", yt.description)
  # print("Ratings: ", yt.rating)

  audio = yt.streams.get_audio_only()

  print("Downloading...")
  audio.download(path, title)
  os.rename(oldName, newName)
  print("Download completed. (path: {})".format(newName))


def main():
  # get urls for download from "yt-dl-urls.txt"
  scriptPath = os.path.dirname(__file__)
  fileName = "urls.txt"
  path = os.path.join(scriptPath, fileName)

  # open file, read lines and close
  f = open(path, "r")
  for url in f.readlines():
    doDownload(url)
    print("")
  f.close()


if __name__ == "__main__":
  main()
