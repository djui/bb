#!/bin/bash

ERL=erl

${ERL} -pa ebin deps/*/ebin \
       -config sys.config \
       -s bb
