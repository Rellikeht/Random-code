#!/bin/sh

LOCK_DIRECTORY=/tmp/batch_lock
if [ -z "$2" ]
then
	WORKING_DIRECTORY=`pwd`
	START_COMMAND=
	MAIN_COMMAND="$1"
	END_COMMAND=
elif [ -z "$3" ]
then
	WORKING_DIRECTORY="$2"
	START_COMMAND=
	MAIN_COMMAND="$1"
	END_COMMAND=
elif [ -z "$4" ]
then
	WORKING_DIRECTORY=`pwd`
	START_COMMAND="$1"
	MAIN_COMMAND="$2"
	END_COMMAND="$3"
else
	WORKING_DIRECTORY="$1"
	START_COMMAND="$2"
	MAIN_COMMAND="$3"
	END_COMMAND="$4"
fi

if [ -z "$THREADS" ]
then
	THREADS=11
fi

if [ -z "$WAIT_TIME" ]
then
	WAIT_TIME=0.1
fi

thread_func ()
{
	FILE="$LOCK_DIRECTORY/th$(date +%s%N)"
	touch "$FILE"
	sleep $WAIT_TIME
	"$@"
	rm "$FILE"
}

free_threads ()
{
	USED=`ls "$LOCK_DIRECTORY" | wc -l`
	echo $(($1-$USED))
}

mkdir -p "$LOCK_DIRECTORY"
if [ -n "$START_COMMAND" ]
then
	"$START_COMMAND"
fi

IFS="
"

for token in `cat /dev/stdin`
do
	FREE_THREADS=$(free_threads $THREADS)
	while [ $FREE_THREADS -le 0 ]
	do
		FREE_THREADS=$(free_threads $THREADS)
		sleep $WAIT_TIME
	done

	thread_func "$MAIN_COMMAND" "$token" &
	sleep $WAIT_TIME

done

while [ -n "`ls $LOCK_DIRECTORY`" ]
do
	sleep $WAIT_TIME
done

if [ -n "$END_COMMAND" ]
then
	"$END_COMMAND"
fi
#rm -r "$LOCK_DIRECTORY"
