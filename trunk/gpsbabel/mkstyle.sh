
for i in style/*.style
do
	A=`basename $i | sed "s/.style$//"`
	[ $A = "README" ] && continue
	ALIST="{\"$A\", $A} , $ALIST"
	echo "static char $A[] = "
	sed 's/"/\\"/g;s/\(^.\)/"\1/g;s/\(.$\)/\1\"/g' $i 
	echo ";"

done

echo "#include \"defs.h\""
echo "style_vecs_t style_list[] = {$ALIST {0,0}};"
