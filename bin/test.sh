#!/bin/bash

sbt clean
sbt "project core; test"
sbt "project example; runAll"
sbt "project genExample; testOnly generated.example.test.generatedExamples"
