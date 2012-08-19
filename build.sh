if [ ! -d bin ]; then
  mkdir bin
fi

scalac -d bin/ -cp lib/gson-2.2.1.jar src/main/*.scala
