#include<stdio.h>
#include<stdlib.h>

#define NUMBERS 100
#define BOARDS  99

typedef struct {
	int cells[25];
} board;

int main(int argc, char **argv) {
	if(argc < 2) {
		fprintf(stderr, "Need a filename\n");
		exit(1);
	}

	FILE *f = fopen(argv[1], "r");

	int numbers[NUMBERS];
	board boards[BOARDS];

	for(int i=0; i<NUMBERS; i++) {
		if(i) fscanf(f, ",");
		fscanf(f, "%d", numbers + i);
	}
	fscanf(f, "\n\n");
	for(int i=0; i<BOARDS; i++) {
		for(int y=0; y<5;y++) {
			for(int x=0; x<5;x++) {
				if(x) fscanf(f, " ");
				fscanf(f, "%d", boards[i].data[x+y*5]);
			}
			fscanf(f, "\n");
		}
		fscanf(f, "\n");
	}
}
