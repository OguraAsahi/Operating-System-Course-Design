#include<stdio.h>
#include<stdlib.h>
#include<sys/types.h>
#include<fcntl.h>

#define BUFFER_SIZE 32
char inputBuf[32], outputBuf[32];
int main(){
	int fd, m, n;
	char c;
	fd = open("/dev/FIFOWithBlock", O_RDWR);
	if(fd < 0){
		printf("open /dev/FIFOWithBlock failed\n");
		exit(-1);
	}
	inputBuf[1] = '\0';
	while(1){
		printf("input a char\n");
		c = getchar();
		if(c == 'q')
			break;
		inputBuf[0] = c;
		n = write(fd, inputBuf, 1 * sizeof(char));
		printf("write done!\n");

	}
	close(fd);
	return 0;
}
