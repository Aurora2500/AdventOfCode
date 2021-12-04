#include <stdlib.h>
#include <stdio.h>

void part1(char *filename) {
	FILE *f = fopen(filename, "r");

	int count = 0;
	int last;
	int curr;
	fscanf(f, "%d\n", &curr);
	do {
		last = curr;
		fscanf(f, "%d\n", &curr);
		if(curr > last) count++;
	} while(!feof(f));
	printf("part 1: %d\n", count);
	fclose(f);
}

void part2(char *filename) {
	FILE *f = fopen(filename, "r");

	int count = 0;
	int sumlast;
	int prel;
	int last;
	int curr;
	fscanf(f, "%d\n", &prel);
	fscanf(f, "%d\n", &last);
	fscanf(f, "%d\n", &curr);
	sumlast = curr + last + prel;
	do {
		prel = last;
		last = curr;
		fscanf(f, "%d\n", &curr);
		int sum = curr + last + prel;
		if(sum > sumlast) count++;
		sumlast = sum;
	} while(!feof(f));
	printf("part 2: %d\n", count);
	fclose(f);
}

int main(int argc, char **argv) {
	if(argc < 2) {
		fprintf(stderr, "Need a filename\n");
		exit(1);
	}

	part1(argv[1]);
	part2(argv[1]);
}
