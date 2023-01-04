#!/bin/bash

fbc -g ./3dviewer.bas
if [[ $? -eq 0 ]]; then
    ./3dviewer
fi
