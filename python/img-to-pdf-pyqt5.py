#!/bin/env python3
#
# work in progress

import sys
from PIL import Image
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QToolTip, QFileDialog
from PyQt5.QtGui import QIcon, QFont


class Window(QWidget):
  def __init__(self):
    super().__init__()
    self.title = "Image to PDF Converter"
    self.image = False
    self.images = []
    self.initUI()

  def initUI(self):
    self.setWindowTitle(self.title)
    self.setGeometry(50, 50, 500, 500)
    QToolTip.setFont(QFont("Arial", 14))

    buttonSelect = QPushButton("Select File", self)
    buttonSelect.setToolTip("Select the files you want to convert.")
    buttonSelect.clicked.connect(self.openFileDialog())

    buttonConvert = QPushButton("Convert", self)
    buttonConvert.setToolTip("Convert the images to a PDF document.")
    buttonConvert.clicked.connect(self.convertToPDF())

  def convertToPDF(self):
    if self.image == False:
      return

    if len(self.images) == 0:
      self.image.save("FriedeMarvin", "PDF", resolution=100.0)
    else:
      self.image.save("FriedeMarvin", "PDF", resolution=100.0,
                      save_all=True, append_images=self.images)

  def openFileDialog(self):
    files, _ = QFileDialog.getOpenFileNames(
        self, "Open files", "", "Image files (*.jpg *.gif *.png)")
    if len(files) == 1:
      self.image = Image.open(files[0])
    elif len(files) > 1:
      self.image = Image.open(files[0])
      for i in range(1, len(files) - 1):
        self.images.append(Image.open(files[i]))


if __name__ == '__main__':
  app = QApplication(sys.argv)
  ex = Window()
  ex.show()
  sys.exit(app.exec_())
