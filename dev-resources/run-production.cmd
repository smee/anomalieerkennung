@echo off
set INPUT_FOLDER=e:\datasets\gt1\DATA_GTI\Downloaded_ProcessedData
set DEMO=true
set DEV=false
set DATABASE_URL=mysql://root@localhost:5029/gt1-db
set DB_NAME=gt1-db
java -jar scada-ui.jar
