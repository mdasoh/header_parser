for f in `find /usr/include -type f | grep "\.h" | egrep -v "hpp"`; do
cpp-3.3 $f newfile.ii
if test -e newfile.ii; then
echo -n $f: 
cat newfile.ii | ./parser | grep -l tax
echo
fi
done
