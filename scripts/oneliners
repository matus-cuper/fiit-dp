# Create CSV file for each meter_id from train.csv
for f in `cut -f 2 -d ',' train.csv | uniq | sort | uniq | grep -v "meter_id"` ; do echo "x,meter_id,Timestamp,Values" > data/${f}.csv ; done

# Fill all CSV files with corresponding data from train.csv
for f in *.csv ; do cat ../train.csv | grep "^[_0-9]\+,${f%.csv}," >> ${f} ; echo "Filtering for file ${f} is completed" ; done

# Remove first and second column
for f in *.csv ; do cut -d ',' -f 3-4 ${f} > ${f}.tmp ; mv ${f}.tmp ${f} ; echo "Removing columns in file ${f} is completed" ; done

# Remove NA
for f in ./* ; do grep -v ",$" $f > $f.tmp ; mv $f.tmp $f ; echo $f ; done
