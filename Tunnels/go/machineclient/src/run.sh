#!/bin/bash
export LOG_LEVEL=Debug
export PORTAL=https://portal.dymium.local:3001/
export KEY=fjRvByVbb7nn
export SECRET=wyh5R2A7DM-qx-FllSb8U8BbZt3z97roRuPExETqVFfm6CBNRgmlWTYNNMMJQhbyHJXDFf9XbJtKqc2yjifVru1YoMUxMQOMrI9MZvjwfQuJF9KawPiHh7oJ8gpPDrR6
export CUSTOMER=spoofcorp
export TUNNELSERVER=portal.dymium.local:15654
export LOCAL_ENVIRONMENT=true
export LISTENER_PORT=6666

./machineclient 2>&1 | tee output.txt

