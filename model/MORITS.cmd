#!/bin/bash
# @ class = short
# @ group = remind-r
# @ resources=ConsumableMemory(3200)
# @ notification = complete
# @ checkpoint = no
# @ restart = no
# @ job_type = serial
# @ input = /dev/null
# @ output = log.txt
# @ error = log.txt
# @ queue


# start gams job
gams /home/andrew/Project/DIETER/DIETERcode/DIETER_v1.2.1.gms
