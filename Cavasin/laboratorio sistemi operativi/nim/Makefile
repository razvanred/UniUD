project: nimServer nimClient
	$(info Built project!)

directories: 
	mkdir -p MakeFiles/visualt.dir
	mkdir -p MakeFiles/nimClient.dir
	mkdir -p MakeFiles/nimServer.dir

visualt.o: directories	
	$(info Building target visualt)
	gcc  -I"./visualt/include" -g -Wall -Wextra -o MakeFiles/visualt.dir/visualt.o   -c "./visualt/src/visualt.c"

visualt: visualt.o
	ar qc MakeFiles/visualt.dir/libvisualt.a  MakeFiles/visualt.dir/visualt.o
	ranlib MakeFiles/visualt.dir/libvisualt.a
	$(info Built target visualt)

nimClient.o: visualt  directories
	$(info Building target nimClient)
	gcc  -I"./visualt/include"  -g   -Wall -Wextra -o MakeFiles/nimClient.dir/client.o   -c "./client.c"

nimClient: nimClient.o visualt
	gcc -g   MakeFiles/nimClient.dir/client.o  -o nimClient  MakeFiles/visualt.dir/libvisualt.a
	$(info Built target nimClient)

nimServer.o:  directories
	$(info Building target nimServer)
	gcc -g -Wall -Wextra -o MakeFiles/nimServer.dir/server.o -c "./server.c"

nimServer: nimServer.o
	gcc -g MakeFiles/nimServer.dir/server.o -o nimServer -lpthread
	$(info Built target nimServer)
