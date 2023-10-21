#!/bin/bash

opt-16 -dot-cfg -disable-output -enable-new-pm=0 jit_test.ll
dot -Tpng -ofunction.png .executecontract.dot
