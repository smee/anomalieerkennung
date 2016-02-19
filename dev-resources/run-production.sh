#!/bin/bash
export INPUT_FOLDER=/home/steffen/datasets/gt1/DATA_GTI/Downloaded_ProcessedData
export DEMO=true
export DEV=false
export DATABASE_URL=mysql://root@localhost:5029/gt1-db
export DB_NAME=gt1-db
java -jar gt1-ui.jar
