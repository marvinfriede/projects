import os

def lowercase_exts(folder):
  for fname in os.listdir(folder):
    name, ext = os.path.splitext(fname)
    os.rename(os.path.join(folder, fname), os.path.join(folder, name + ext.lower()))

lowercase_exts(".")