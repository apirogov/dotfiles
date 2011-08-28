#!/bin/sh
sensors | grep CPU | tail -n 1 | sed -e 's/CPU Temperature:    +//' | sed -e 's/  (.*//'
