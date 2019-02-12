#!/bin/sh
if [ $# -lt 1 ]
then
  echo "缺少参数：procedure_name"
  exit 1
fi
 
PINFO=`ps -ef|grep $1`
echo $PINFO
PROCESS=`ps -ef|grep $1|grep erlang|grep -v grep|grep -v PPID|awk '{ print $2}'`
for i in $PROCESS
do
  echo "Kill the $1 process [ $i ]"
  kill -9 $i
done
