#!/bin/bash

ERL=erl

${ERL} -pa ebin \
       -config sys.config \
       -s bb
