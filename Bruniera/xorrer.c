#include<stdio.h>
#include<stdlib.h>
#include<errno.h>
#include<unistd.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<fcntl.h>
#include<string.h>
#include<time.h>
#include<inttypes.h>


//l'estensione originale era .mp4


void read_key(char* file, uint8_t* key){
	int fd;
	
	if((fd=open (file, O_RDWR, S_IRWXU))==-1){     //apro il file chiave
		perror("open");
		exit(0);
	}
	
	if(read(fd, key, 8)==-1){                     //leggi la chiave dal file chiave
		perror("read");
		exit(0);
	}	
}

void cypher(int fd, uint8_t key[], uint8_t state){
	int i=0;
	uint8_t symbol;
	uint8_t previous;
	
	while(read(fd, &symbol, 1)>0){
		previous = symbol;
		
		//printf("%c, %c, %c\n", symbol, state, key[i % 8]);
		symbol = symbol ^ (state ^ key[i % 8]);       //modifica simbolo e aggiorna stato
		state = previous + (state + key[i % 8]);      //aggiorna stato
		
		if(lseek(fd, -1, SEEK_CUR)==-1){     
			perror("seek");
			exit(0);
		}
		
		if(write(fd, &symbol, 1)==-1){           //sovrascrivi simbolo
			perror("write");
			exit(0);
		}
		i++;
	}
	if(fd==-1){
		perror("read");
		exit(0);
	}
}

void decypher(int fd, uint8_t key[], uint8_t state){
	int i=0;
	uint8_t symbol;
	uint8_t previous;
	
	while(read(fd, &symbol, 1)>0){
		//printf("%c, %c, %c", symbol, state, key[i % 8]);
		
		symbol = symbol ^ (state ^ key[i % 8]);       //modifica simbolo
		state = symbol + (state + key[i % 8]);        //ricrea stato
		
		if(lseek(fd, -1, SEEK_CUR)==-1){     
			perror("seek");
			exit(0);
		}
		
		if(write(fd, &symbol, 1)==-1){           //sovrascrivi simbolo
			perror("write");
			exit(0);
		}
		i++;
	}
	if(fd==-1){
		perror("read");
		exit(0);
	}
}

int main(int argv, char**argc){             // arg[1]=testo arg[2]=chiave
	int fd;
	uint8_t initial_state = 0;
	uint8_t key[8];
	
	if(argv<5){                             //devono esserci almeno 5 argomenti
		printf("invalid file\n");
		exit(0);
	}
	
	read_key(argc[2], key);
	
	close(fd);
	
	if((fd=open (argc[1], O_RDWR, S_IRWXU))==-1){ //apre il file di testo
		perror("open");
		exit(0);
	}
	
	initial_state = atoi(argc[4]);
	
	if(argc[3][0] == 'c'){
		cypher(fd, key, initial_state);
	}
	else if(argc[3][0] == 'd'){
		decypher(fd, key, initial_state);
	} else {
		perror("invalid operation");
		exit(0);
	}
	
	close(fd);
	return 0;
}
