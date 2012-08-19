# First parameter is products, second is listings, third is output.

# Eg.
#
# ./go.sh products.txt listings.txt results.txt

scala -cp lib/gson-2.2.1.jar -sourcepath bin Main $1 $2 $3