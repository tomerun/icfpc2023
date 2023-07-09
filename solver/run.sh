#!/bin/bash -exu

IDX=${AWS_BATCH_JOB_ARRAY_INDEX:-0}
SEED_START=$(expr $IDX \* $RANGE + 1)
SEED_END=$(expr $IDX \* $RANGE + $RANGE + 1)
if [ $SEED_END -gt 91 ]
then
	SEED_END=91
fi

aws s3 cp s3://marathon-tester/ICFPC2023/problem.zip problem.zip
unzip problem.zip
mkdir result

crystal build --release solver.cr
for (( i = $SEED_START; i < $SEED_END; i++ )); do
	seed=$(printf "%04d" $i)
	echo "seed:$seed"
	./solver < problem/"$seed".json > result/"$seed".json 2> >(tee -a log.txt >&2)
done

aws s3 cp log.txt s3://marathon-tester/$RESULT_PATH/$(printf "%04d" $SEED_START).txt
aws s3 cp result/*.json s3://marathon-tester/$RESULT_PATH/
