for i in *cc *.h
do
  git blame $i | egrep -i '2013.*c\+\+'
done
