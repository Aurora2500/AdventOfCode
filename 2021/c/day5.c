#include<stdlib.h>
#include<stdio.h>
#include<string.h>

#include"common.h"

#define BOARD_SZ 1000

int board_1[BOARD_SZ][BOARD_SZ];
int board_2[BOARD_SZ][BOARD_SZ];


int main() {
	if(argc < 2) {
		fprintf(stderr, "Need a filename\n");
		exit(1);
	}

	memset(board_1, 0, BOARD_SZ*BOARD_SZ);
	memset(board_2, 0, BOARD_SZ*BOARD_SZ);

	FILE *f = fopen(argv[1]);


}
