#!/bin/bash

# Usage:
# First argument: directory with tests and output files
# Use -c as second argument to clear 'tester' working directory 

RED='\033[0;31m';
GREEN='\033[0;32m';
YELLOW='\033[0;33m';
BLUE='\033[0;34m';
MAGENTA='\033[0;35m';
CYAN='\033[0;36m'
NC='\033[0m';

mkdir -p tester
goodCnt=0
runCnt=0
diffCnt=0
failCnt=0
compCnt=0
for file in $1/*.lat $1/**/*.lat; do
    fileNoExt="${file%.*}"
    name="${file##*/}"
    noExt="${name%.*}"
    if [[ $noExt == "*" ]]; then
        continue
    fi
    printf "\n${BLUE}TESTING: $noExt ${NC}\n"
    res="$(./latc $file -o ./tester/$noExt 2>&1)"
    if [[ $res =~ "OK" ]]; then
        cpres="$(cp $fileNoExt.output ./tester/$noExt.output)"
        if [[ -f "$fileNoExt.input" ]]; then
            cpres="$(cp $fileNoExt.input ./tester/$noExt.input)"
            sed 's/\r$//' "./tester/$noExt.input" > "./tester/tmp.input"
            runres="$(./tester/${noExt} <./tester/tmp.input 1>./tester/tmp.output)"
            if [[ $runres -ne 0 ]]; then
                runCnt=$((runCnt+1))
                printf "${ORANGE}RUNTIME ERROR${NC}\n"
            fi
        else
            runres="$(./tester/${noExt} 1>./tester/tmp.output)"
            if [[ $runres -ne 0 ]]; then
                runCnt=$((runCnt+1))
                printf "${ORANGE}RUNTIME ERROR${NC}\n"
            fi
        fi
        sed 's/\r$//' "./tester/$noExt.output" > "./tester/exptmp.output"
        cmp -s "./tester/tmp.output" "./tester/exptmp.output"
        ret=$?
        if [[ ret -ne 0 ]]; then
            diffCnt=$((diffCnt+1))
            printf "${MAGENTA}EXPECTED & ACTUAL OUTPUT DIFFER${NC}\n"
        else
            goodCnt=$((goodCnt+1))
            printf "${GREEN}PASS${NC}\n";
        fi
    elif [[ $res =~ "ERROR" ]]; then
        failCnt=$((failCnt+1))
        printf "${RED}FAIL${NC} ${res}\n";
    else
        compCnt=$((compCnt+1))
        printf "${CYAN}COMPILER DID NOT RETURN OK OR ERROR${NC}\n"
    fi
    printf "\n${BLUE}<=======---=======>${NC}\n"
done

if [[ $* == *-c* ]]; then
    rm ./tester/*
fi

printf "\n<=======RESULTS=======>\n"
printf "${GREEN}PASSED: ${goodCnt} ${NC}\n"
printf "${YELLOW}RUNTIME ERROR: ${runCnt} ${NC}\n"
printf "${MAGENTA}OUTPUT DIFFERENCE: ${diffCnt} ${NC}\n"
printf "${RED}FAIL: ${failCnt} ${NC}\n"
printf "${CYAN}COMPILER ERROR: ${compCnt} ${NC}\n"

