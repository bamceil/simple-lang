#!/usr/bin/python3
import os

files = []

for file in os.listdir("."):
    if file.endswith(".lang"):
        files.append(file)

for file in files:
    output = file.split('.')[0] + '.bc'
    run = '../bin/lang ' + file + ' ' + output
    os.system(run)
    os.system('lli ' + output)