if [ $# -gt 0 ]
then
  day=$(printf "%02d" $1)
else
  day=-1
fi

if [ $day -gt 0 ]
then
	if test -f Day$day.hs;
	then
		ghc Day$day.hs -O2 -Wall
		./Day$day
		rm Day$day.hi
		rm Day$day.o
		rm Day$day
		rm -f AdventHelper.hi
		rm -f AdventHelper.o
	else
		echo "Day " $day " does not exist"
	fi
else
	missing=""
	for i in $(seq -f "%02g" 1 25)
	do
		if test -f Day$i.hs;
		then
			ghc Day$i.hs -O2 > /dev/null
			./Day$i
			rm Day$i.hi
			rm Day$i.o
			rm Day$i
		else
			if [ "$missing" = "" ]
			then
				missing=$i
			else
				missing=$missing","$i
			fi
		fi
	done
	rm -f AdventHelper.hi
	rm -f AdventHelper.o
	if [ "$missing" != "" ]
	then
		echo "Missing days = ["$missing"]"
	fi
fi
